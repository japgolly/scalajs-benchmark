package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.engine._
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
import scala.util.{Success, Try}

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
      init(p.suite, p.guiOptions, true)

    def init[P](suite: GuiSuite[P], guiOptions: GuiOptions, respectDisabledByDefault: Boolean): State[P] =
      State[P](
        status        = SuitePending,
        editors       = suite.params.initialState,
        disabledBMs   = if (respectDisabledByDefault) initDisabledBMs(suite.suite.bms) else Set.empty,
        oldTitle      = None,
        formatResults = guiOptions.formatResultsDefault)
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
        _.bm.getOrElse(k, BMStatus.Pending))(
        s => r => r.copy(bm = r.bm.updated(k, s)))

    def at[P](k: PlanKey[P]): Optional[SuiteStatus[P], BMStatus] =
      running ^|-> runningAt(k)
  }

  // ===================================================================================================================

  /** @return An [[AsyncCallback]] that completes when BMs actually start, and returns a new [[AsyncCallback]] that
    *         completes when the entire suite of BMs is finished.
    */
  def run(guiPlan: GuiPlan)
         ($      : StateAccessPure[State[guiPlan.Param]],
          options: EngineOptions): AsyncCallback[AsyncCallback[SuiteDone[guiPlan.Param]]] = {
    type P = guiPlan.Param
    import guiPlan.guiSuite
    val plan = guiPlan.plan

    def actuallyStart(startTime: Long, onEnd: Try[SuiteDone[P]] => Callback): CallbackTo[AbortFn] =
      Engine.run(plan, options) {

        case BenchmarkPreparing(_, k) =>
          $.modStateAsync(State.at(k) set BMStatus.Preparing)

        case BenchmarkRunning(_, k) =>
          $.modStateAsync(State.at(k) set BMStatus.Running)

        case BenchmarkFinished(p, k, r) =>
          val setResult = State.at(k) set BMStatus.Done(r)
          val setProgress = State.status[P].modify {
            case sr: SuiteRunning[P] => sr.copy(progess = p)
            case x                   => x
          }
          $.modStateAsync(setResult compose setProgress)

        case SuiteStarting(_) =>
          AsyncCallback.unit

        case SuiteFinished(progress) =>
          def modFn(endTime: Long, s: SuiteStatus[P]): SuiteDone[P] = {
            val bm = SuiteStatus.running.getOption(s).map(_.bm).getOrElse(Map.empty)
            val time = FiniteDuration(endTime - startTime, MILLISECONDS)
            SuiteDone(guiSuite, progress, bm, time)
          }
          for {
            endTime <- AsyncCallback.delay(System.currentTimeMillis())
            s1      <- $.state.asAsyncCallback
            done    <- AsyncCallback.pure(modFn(endTime, s1.status))
            _       <- $.setStateAsync(s1.copy(status = done))
            _       <- onEnd(Success(done)).asAsyncCallback
          } yield ()
      }

    for {
      startTime <- AsyncCallback.delay(System.currentTimeMillis())
      promise   <- AsyncCallback.promise[SuiteDone[P]].asAsyncCallback
      _         <- $.modStateAsync(State.status.set(SuiteWillStart)(_))
      abort     <- actuallyStart(startTime, promise._2).asAsyncCallback
      running   <- AsyncCallback.delay(SuiteRunning[P](guiSuite, Progress.start(plan, options), Map.empty, abort))
      _         <- $.modStateAsync(State.status set running)
    } yield promise._1
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
          .collect { case BMStatus.Done(Right(s)) => s.average }
          .reduceOption(_.min(_))
          .getOrElse(Duration.Zero)

      val mainFmt = FormatResult.choose(minAvg)
      Vector(mainFmt, FormatResult.OpsPerSec)
    }

    // =================================================================================================================
    // Rendering

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
          params <- params.parseState(ev.value).toOption.filter(_.nonEmpty)
          bms    <- Some(selectedBMs).filter(_.nonEmpty)
        } yield {
          val guiSuite2 = p.suite.withBMs(bms)
          val guiPlan   = GuiPlan(guiSuite2)(params)
          val runCB     = run(guiPlan)($, p.engineOptions).toCallback
          val eta       = guiPlan.etaMs(p.engineOptions)
          (runCB, eta)
        }
      }

      val onStart: Option[Callback] =
        startData.map(_._1)

      val etaOption: Option[Double] =
        startData.map(_._2)

      def renderETA = {
        val eta = etaOption.fold("-")(GuiUtil.formatETA)
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
          <.span("Benchmark running... ETA: ", GuiUtil.formatETA(eta)),
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
          <.span(s"Benchmark completed in ${GuiUtil.formatETA(r.totalTime)}."),
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
        m.getOrElse(k, BMStatus.Pending) match {
          case BMStatus.Done(Right(stats)) => fmt.score.getDouble(stats.score) getOrElse 0
          case BMStatus.Done(Left(_))
             | BMStatus.Pending
             | BMStatus.Running
             | BMStatus.Preparing => -0.1 // 0 puts a thick bar above the axis which looks like a small result
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
  }
}
