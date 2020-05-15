package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.engine._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.benchmark.gui.GuiParams.GenState
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.components.TriStateCheckbox
import japgolly.scalajs.react.vdom.html_<^._
import monocle._
import monocle.macros.Lenses
import org.scalajs.dom.document
import scala.concurrent.duration._
import scalacss.ScalaCssReact._
import Styles.{Suite => *}
import japgolly.scalajs.benchmark.vendor.chartjs.Chart

final class SuiteRunner[P] {
  import SuiteRunner._

  val Component = ScalaComponent.builder[Props[P]]
    .initialStateFromProps(State.init[P])
    .renderBackend[Backend[P]]
    .componentDidMount(_.backend.onMount)
    // TODO handle suite changes - it's all in state atm
    .componentWillUnmount(_.backend.shutdown)
    .build
}

object SuiteRunner {

  def render[P](suite        : GuiSuite[P],
                engineOptions: EngineOptions = EngineOptions.default,
                guiOptions   : GuiOptions    = GuiOptions.default): VdomElement =
    apply[P].Component(Props(suite, engineOptions, guiOptions))

  def apply[P]: SuiteRunner[P] =
    instance.asInstanceOf[SuiteRunner[P]]

  private val instance =
    new SuiteRunner[Any]

  final case class Props[P](suite        : GuiSuite[P],
                            engineOptions: EngineOptions,
                            guiOptions   : GuiOptions)

  @Lenses
  final case class State[A](status       : SuiteStatus[A],
                            editors      : GenState,
                            disabledBMs  : Set[Int],
                            oldTitle     : Option[String],
                            formatResults: FormatResults)

  object State {
    def at[A](k: PlanKey[A]): Optional[State[A], BMStatus] =
      status ^|-? SuiteStatus.at[A](k)

    def initDisabledBMs(bms: Vector[Benchmark[Nothing]]): Set[Int] =
      bms.iterator.zipWithIndex.filter(_._1.isDisabledByDefault).map(_._2).toSet

    def init[P](p: Props[P]): State[P] =
      State[P](
        status        = SuitePending,
        editors       = p.suite.params.initialState,
        disabledBMs   = initDisabledBMs(p.suite.suite.bms),
        oldTitle      = None,
        formatResults = p.guiOptions.formatResultsDefault)
  }

  type EachBMStatus[P] = Map[PlanKey[P], BMStatus]

  sealed trait SuiteStatus[+P]
  case object SuitePending   extends SuiteStatus[Nothing]
  case object SuiteWillStart extends SuiteStatus[Nothing]

  final case class SuiteRunning[P](suite  : GuiSuite[P],
                                   progess: Progress[P],
                                   bm     : EachBMStatus[P],
                                   abortFn: AbortFn) extends SuiteStatus[P] {
    @inline def plan = progess.plan
  }

  final case class SuiteDone[P](suite    : GuiSuite[P],
                                progess  : Progress[P],
                                bm       : EachBMStatus[P],
                                totalTime: FiniteDuration) extends SuiteStatus[P] {
    @inline def plan = progess.plan
  }

  object SuiteStatus {
    def running[P]: Prism[SuiteStatus[P], SuiteRunning[P]] =
      Prism[SuiteStatus[P], SuiteRunning[P]] { case r: SuiteRunning[P] => Some(r); case _ => None }(s => s)

    def runningAt[P](k: PlanKey[P]): Lens[SuiteRunning[P], BMStatus] =
      Lens[SuiteRunning[P], BMStatus](
        _.bm.getOrElse(k, BMPending))(
        s => r => r.copy(bm = r.bm.updated(k, s)))

    def at[P](k: PlanKey[P]): Optional[SuiteStatus[P], BMStatus] =
      running ^|-> runningAt(k)
  }

  // ===================================================================================================================

  final class Backend[P]($: BackendScope[Props[P], State[P]]) {
    type Props        = SuiteRunner.Props[P]
    type State        = SuiteRunner.State[P]
    type SuiteStatus  = SuiteRunner.SuiteStatus[P]
    type SuiteRunning = SuiteRunner.SuiteRunning[P]
    type SuiteDone    = SuiteRunner.SuiteDone[P]

    private type ResultFmts = Vector[FormatResult]

    def onMount: Callback = {
      def storeCurrentTitle =
        CallbackTo(document.title) |> Some.apply |> State.oldTitle[P].set

      def setNewTitle =
        $.props.map(p => document.title = p.suite.name)

      storeCurrentTitle.flatMap($.modState(_, setNewTitle))
    }

    def shutdown: Callback = {
      def abortIfRunning: SuiteStatus => Callback = {
        case r: SuiteRunning => r.abortFn.callback
        case _: SuiteDone
           | SuitePending
           | SuiteWillStart  => Callback.empty
      }

      def restoreTitle(o: Option[String]): Callback =
        o.fold(Callback.empty)(t => Callback(document.title = t))

      $.state.flatMap(s =>
        abortIfRunning(s.status) >> restoreTitle(s.oldTitle))
    }

    // =================================================================================================================
    // Util

    private val guiSuiteBMs =
      GuiSuite.suite[P] ^|-> Suite.bms

    private val updateEditorState: (Option[GenState], Callback) => Callback =
      (os, cb) => $.modStateOption(t => os.map(State.editors.set(_)(t)), cb)

    private def toggleBM(i: Int): Callback =
      $.modState(State.disabledBMs.modify(s =>
        if (s contains i) s - i else s + i)(_))

    private def makeSoleBM(i: Int): Callback =
      $.props.flatMap(p =>
        $.modState(State.disabledBMs.set(
          p.suite.suite.bms.indices.toSet - i)(_)))

    private def deriveResultFmts(progress: Progress[P], m: EachBMStatus[P]): ResultFmts = {
      val keys = progress.plan.keys

      val minAvg =
        keys
          .iterator
          .flatMap(m.get)
          .collect { case BMDone(Right(s)) => s.average }
          .reduceOption(_.min(_))
          .getOrElse(Duration.Zero)

      val mainFmt = FormatResult.choose(minAvg)
      Vector(mainFmt, FormatResult.OpsPerSec)
    }

    // =================================================================================================================
    // Rendering

    private def formatETA(ms: Double): String = {
      val sec = ms / 1000 + 0.5 // adding 0.5 for rounding
      val min = sec / 60
      val hr  = min / 60
      s"%d:%02d:%02d".format(hr.toInt, (min % 60).toInt, (sec % 60).toInt)
    }

    private def renderSuitePending(p: Props, s: State): VdomElement = {
      val ev = StateSnapshot(s.editors)(updateEditorState)
      val params = p.suite.params
      val th = <.th(*.settingsTableHeader)
      val td = <.td(*.settingsTableData)

      def bmRow = {

        val bms = p.suite.suite.bms

        val allCheckbox =
          TagMod.when(bms.length > 2) {
            val triState =
              if (s.disabledBMs.isEmpty)
                TriStateCheckbox.Checked
              else if (s.disabledBMs.size == bms.length)
                TriStateCheckbox.Unchecked
              else
                TriStateCheckbox.Indeterminate

            val setNextState: Callback =
              $.modState(State.disabledBMs[P].set(
                triState.nextDeterminate match {
                  case TriStateCheckbox.Checked   => Set.empty
                  case TriStateCheckbox.Unchecked => bms.indices.toSet
                }
              ))

            <.label(
              *.allBMsCheckbox,
              TriStateCheckbox.Props(triState, setNextState).render,
              "All")
          }

        val checkboxes =
          bms.iterator.zipWithIndex.toTagMod { case (bm, i) =>
            <.label(
              *.settingsTableBm,
              ^.onDoubleClick --> makeSoleBM(i),
              <.input.checkbox(
                ^.checked  := !s.disabledBMs.contains(i),
                ^.onChange --> toggleBM(i)),
              <.span(
                *.settingsTableBmLabel,
                bm.name))
          }

        <.tr(
          th("Benchmarks"),
          td(allCheckbox, checkboxes))
      }

      def paramRow(i: Int) =
        <.tr(
          th(params.headers(i)),
          td(params.editors(i)(ev)))

      def paramRows: TagMod =
        if (params.headers.isEmpty)
          EmptyVdom
        else
          TagMod(params.headers.indices.map(paramRow): _*)

      val startData = {
        def selectedBMs = p.suite.suite.bms.iterator
          .zipWithIndex
          .filterNot(s.disabledBMs contains _._2)
          .map(_._1)
          .toVector

        for {
          ps <- params.parseState(ev.value).toOption.filter(_.nonEmpty)
          bms <- Some(selectedBMs).filter(_.nonEmpty)
        } yield {
          val s2 = guiSuiteBMs.set(bms)(p.suite)
          val cb = start(s2, p.engineOptions, ps).toCallback
          val eta = p.engineOptions.estimatedMsPerBM * (ps.length * bms.length)
          (cb, eta)
        }
      }

      val onStart: Option[Callback] =
        startData.map(_._1)

      val etaOption: Option[Double] =
        startData.map(_._2)

      def renderETA = {
        val eta = etaOption.fold("-")(formatETA)
        <.div(*.etaRow, "ETA: ", eta)
      }

      def startButton =
        <.button(
          *.startButton,
          ^.disabled := onStart.isEmpty,
          ^.onClick -->? onStart,
          "Start")

      <.div(
        renderFormatButtons(p, s),
        renderETA,
        <.table(
          *.settingsTable,
          <.tbody(
            bmRow,
            paramRows)),
        startButton)
    }

    private def renderSuiteRunning(p: Props, s: State, r: SuiteRunning): VdomElement = {
      val resultFmts = deriveResultFmts(r.progess, r.bm)

      val eta =
        p.engineOptions.estimatedMsPerBM * r.progess.remaining

      def abortButton =
        <.button(
          *.abortButton,
          ^.onClick --> r.abortFn.callback,
          "Abort")

      <.div(
        <.div(*.runningRow,
          <.span("Benchmark running... ETA: ", formatETA(eta)),
          abortButton),
        renderFormatButtons(p, s),
        renderResults(s.formatResults, p.suite, r.progess, r.bm, resultFmts, p.guiOptions),
        renderGraph(p.suite, r.progess, r.bm, resultFmts))
    }

    private def renderSuiteDone(p: Props, s: State, r: SuiteDone): VdomElement = {
      val resultFmts = deriveResultFmts(r.progess, r.bm)

      def resetButton =
        <.button(
          *.resetButton,
          ^.onClick --> $.modState(State.status.set(SuitePending)(_)),
          "Reset")

      <.div(
        <.div(*.doneRow,
          <.span(s"Benchmark completed in ${formatETA(TimeUtil.toMs(r.totalTime))}."),
          resetButton),
        renderFormatButtons(p, s),
        renderResults(s.formatResults, p.suite, r.progess, r.bm, resultFmts, p.guiOptions),
        renderGraph(p.suite, r.progess, r.bm, resultFmts))
    }

    private def renderFormatButtons(p: Props, s: State) =
      <.div(
        *.resultFormatRow,
        "Result format: ",
        p.guiOptions.formatResults.toTagMod { f =>
          <.label(
            *.resultFormat,
            <.input.radio(
              ^.checked := (s.formatResults == f),
              ^.onChange --> $.modState(_.copy(formatResults = f))),
            f.label)
        }
      )
    private def renderResults(fmt       : FormatResults,
                              suite     : GuiSuite[P],
                              progress  : Progress[P],
                              results   : EachBMStatus[P],
                              resultFmts: ResultFmts,
                              guiOptions: GuiOptions): VdomElement =
      fmt.render(FormatResults.Args(suite, progress, results, resultFmts, guiOptions))

    private def renderGraph(suite: GuiSuite[P], progress: Progress[P], m: EachBMStatus[P], resultFmts: ResultFmts): VdomElement = {
      import ReactChart._
      val keys = progress.plan.keys
      val fmt = resultFmts.head
      val bmsToShow = m.size max 1

      val bmFullName: PlanKey[P] => String =
        if (progress.plan.params.length > 1)
          k => k.bm.name + suite.params.bmNameSuffix(k.param)
        else
          _.bm.name

      val titles = keys.iterator
        .map(bmFullName)
        .take(bmsToShow)
        .toVector

      val dataPoints = keys.iterator.map[Chart.Value](k =>
        m.getOrElse(k, BMPending) match {
          case BMDone(Right(stats)) => fmt.score.getDouble(stats.score) getOrElse 0
          case BMDone(Left(_))
             | BMPending
             | BMRunning
             | BMPreparing => -0.1 // 0 puts a thick bar above the axis which looks like a small result
        }
      ).take(bmsToShow).toVector

      val dataset = ScalaDataset(fmt.header, dataPoints)
      val bardata = ScalaBarData(titles, Vector(dataset))
      val props   = ReactChart.Props(*.graph, *.graphInner(bardata))

      <.div(*.graphContainer,
        <.div(*.graphHeader, fmt.graphHeader),
        props.render)
    }

    private def renderDesc(e: VdomElement) =
      <.div(*.suiteDesc, e)

    def render(p: Props, s: State): VdomElement = {
      val inner: VdomElement = s.status match {
        case SuitePending    => renderSuitePending(p, s)
        case SuiteWillStart  => <.span
        case r: SuiteRunning => renderSuiteRunning(p, s, r)
        case r: SuiteDone    => renderSuiteDone(p, s, r)
      }
      <.div(
        <.h2(*.suiteName, p.suite.name),
        p.suite.desc.whenDefined(renderDesc),
        inner)
    }

    // =================================================================================================================

    private def start(suite: GuiSuite[P], options: EngineOptions, params: Vector[P]): AsyncCallback[Unit] = {
      val plan = Plan(suite.suite, params)

      def actuallyStart(startTime: Long) =
        Engine.run(plan, options) {

          case BenchmarkPreparing(_, k) =>
            $.modStateAsync(State.at(k) set BMPreparing)

          case BenchmarkRunning(_, k) =>
            $.modStateAsync(State.at(k) set BMRunning)

          case BenchmarkFinished(p, k, r) =>
            val setResult = State.at(k) set BMDone(r)
            val setProgress = State.status[P].modify {
              case sr: SuiteRunning => sr.copy(progess = p)
              case x                => x
            }
            $.modStateAsync(setResult compose setProgress)

          case SuiteStarting(_) =>
            AsyncCallback.unit

          case SuiteFinished(progress) =>
            val endTime = System.currentTimeMillis()
            $.modStateAsync(State.status.modify { (s: SuiteStatus) =>
              val bm = SuiteStatus.running.getOption(s).map(_.bm).getOrElse(Map.empty)
              val time = FiniteDuration(endTime - startTime, MILLISECONDS)
              SuiteDone(suite, progress, bm, time)
            }(_))
        }

      for {
        startTime <- AsyncCallback.delay(System.currentTimeMillis())
        _         <- $.modStateAsync(State.status.set(SuiteWillStart)(_))
        abort     <- actuallyStart(startTime).asAsyncCallback
        running   <- AsyncCallback.delay(SuiteRunning[P](suite, Progress.start(plan, options), Map.empty, abort))
        _         <- $.modStateAsync(State.status set running)
      } yield ()
    }
  }
}