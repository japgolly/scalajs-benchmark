package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.engine._
import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle._
import monocle.macros.Lenses
import org.scalajs.dom.document
import scala.concurrent.duration._
import scalacss.ScalaCssReact._
import GuiParams.GenState
import ReactTemp._
import Styles.{Suite => *}

/**
  * React component that provides the GUI over a [[GuiSuite]].
  */
object SuiteComp {
  type Comp[P] = ScalaComponent[Props[P], State[P], Backend[P], CtorType.Props]

  case class Props[P](suite: GuiSuite[P], options: Options = Options.Default)

  sealed abstract class ResultFormat(final val label: String)
  object ResultFormat {
    case object Table extends ResultFormat("Table")
    case object Text  extends ResultFormat("Text")
    val all = Vector[ResultFormat](Table, Text)
  }

  @Lenses
  case class State[A](status      : SuiteStatus[A],
                      editors     : GenState,
                      disabledBMs : Set[Int],
                      oldTitle    : Option[String],
                      resultFormat: ResultFormat)

  object State {
    def at[A](k: PlanKey[A]): Optional[State[A], BMStatus] =
      status ^|-? SuiteStatus.at[A](k)

    def initDisabledBMs(bms: Vector[Benchmark[Nothing]]): Set[Int] =
      bms.iterator.zipWithIndex.filter(_._1.isDisabledByDefault).map(_._2).toSet

    def init[P](p: Props[P]): State[P] =
      State[P](
        status       = SuitePending,
        editors      = p.suite.params.initialState,
        disabledBMs  = initDisabledBMs(p.suite.suite.bms),
        oldTitle     = None,
        resultFormat = ResultFormat.Table)
  }

  type EachBMStatus[P] = Map[PlanKey[P], BMStatus]

  sealed trait SuiteStatus[+P]
  case object SuitePending   extends SuiteStatus[Nothing]
  case object SuiteWillStart extends SuiteStatus[Nothing]

  case class SuiteRunning[P](suite: GuiSuite[P], progess: Progress[P], bm: EachBMStatus[P], abortFn: AbortFn) extends SuiteStatus[P] {
    @inline def plan = progess.plan
  }

  case class SuiteDone[P](suite: GuiSuite[P], progess: Progress[P], bm: EachBMStatus[P], totalTime: FiniteDuration) extends SuiteStatus[P] {
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

  sealed trait BMStatus
  case object BMPending extends BMStatus
  case object BMPreparing extends BMStatus
  case object BMRunning extends BMStatus
  final case class BMDone(result: Result) extends BMStatus

  private type ResultFmts    = Vector[ResultFmt]
  private val resultFmtCount = 2
  private val resultBlock1   = ^.colSpan := 3
  private val resultBlockAll = ^.colSpan := (3 * resultFmtCount)

  private val resultTD = <.td(*.resultData)

  private val PlusMinusCell = resultTD("±")

  private val runsCellNone    = resultTD
  private val whenBMPending   = Vector[VdomTag](runsCellNone, resultTD(resultBlockAll))
  private val whenBMPreparing = Vector[VdomTag](runsCellNone, resultTD(resultBlockAll, *.preparing, "Preparing…"))
  private val whenBMRunning   = Vector[VdomTag](runsCellNone, resultTD(resultBlockAll, *.running, "Running…"))

  private def formatTotalTime(fd: FiniteDuration): String =
    Util.addThousandSeps("%.2f" format ResultFmt.getUnits(SECONDS)(fd)) + " seconds"

  final class Backend[P]($: BackendScope[SuiteComp.Props[P], SuiteComp.State[P]]) {
    type Props        = SuiteComp.Props[P]
    type State        = SuiteComp.State[P]
    type SuiteRunning = SuiteComp.SuiteRunning[P]
    type SuiteDone    = SuiteComp.SuiteDone[P]

    private val guiSuiteBMs = GuiSuite.suite[P] ^|-> Suite.bms

    private val updateEditorState: (Option[GenState], Callback) => Callback =
      (os, cb) => $.modStateOption(t => os.map(State.editors.set(_)(t)), cb)

    private def start(suite: GuiSuite[P], options: Options, ps: Vector[P]): AsyncCallback[Unit] = {
      val plan = Plan(suite.suite, ps)

      def actuallyStart(startTime: Long) =
        Engine.run(plan, options) {

          case BenchmarkPreparing(_, k) =>
            $.modStateAsync(State.at(k) set BMPreparing)

          case BenchmarkRunning(_, k) =>
            $.modStateAsync(State.at(k) set BMRunning)

          case BenchmarkFinished(_, k, r) =>
            $.modStateAsync(State.at(k) set BMDone(r))

          case SuiteStarting(_) =>
            AsyncCallback.unit

          case SuiteFinished(progress) =>
            val endTime = System.currentTimeMillis()
            $.modStateAsync(State.status.modify { (s: SuiteStatus[P]) =>
              val bm = SuiteStatus.running.getOption(s).map(_.bm).getOrElse(Map.empty)
              val time = FiniteDuration(endTime - startTime, MILLISECONDS)
              SuiteDone(suite, progress, bm, time)
            }(_))
        }

      for {
        startTime <- AsyncCallback.point(System.currentTimeMillis())
        _         <- $.modStateAsync(State.status.set(SuiteWillStart)(_))
        abort     <- actuallyStart(startTime).asAsyncCallback
        running    = SuiteRunning[P](suite, Progress(plan, 0), Map.empty, abort)
        _         <- $.modStateAsync(State.status set running)
        } yield ()
    }

    private def toggleBM(i: Int): Callback =
      $.modState(State.disabledBMs.modify(s =>
        if (s contains i) s - i else s + i)(_))

    private def makeSoleBM(i: Int): Callback =
      $.props >>= (p =>
        $.modState(State.disabledBMs.set(
          p.suite.suite.bms.indices.toSet - i)(_)))

    private def renderSuitePending(p: Props, s: State): VdomElement = {
      val ev = StateSnapshot(s.editors)(updateEditorState)
      val params = p.suite.params
      val th = <.th(*.settingsTableHeader)
      val td = <.td(*.settingsTableData)

      def bmRow = {
        def bmCell(bm: Benchmark[P], i: Int) =
          <.label(
            *.settingsTableBm,
            ^.onDoubleClick --> makeSoleBM(i),
            <.input.checkbox(
              ^.checked  := !s.disabledBMs.contains(i),
              ^.onChange --> toggleBM(i)),
            <.span(
              *.settingsTableBmLabel,
              bm.name))

        <.tr(
          th("Benchmarks"),
          td(p.suite.suite.bms.iterator.zipWithIndex.map((bmCell _).tupled).toList: _*))
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

      val onStart: Option[Callback] = {
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
          start(s2, p.options, ps).toCallback
        }
      }

      def startButton =
        <.button(
          *.startButton,
          ^.disabled := onStart.isEmpty,
          ^.onClick -->? onStart,
          "Start")

      <.div(
        renderFormatButtons(s),
        <.table(
          *.settingsTable,
          <.tbody(
            bmRow,
            paramRows)),
        startButton)
    }

    private def renderResults(fmt: ResultFormat, suite: GuiSuite[P], progress: Progress[P], m: EachBMStatus[P], resultFmts: ResultFmts): VdomElement =
      fmt match {
        case ResultFormat.Table => renderResultTable(suite, progress, m, resultFmts)
        case ResultFormat.Text  => renderResultText(suite, progress, m, resultFmts)
      }

    private def renderResultText(suite: GuiSuite[P], progress: Progress[P], m: EachBMStatus[P], resultFmts: ResultFmts): VdomElement = {
      val keys = progress.plan.keys

      val rowBuilder = List.newBuilder[Vector[String]]

      def header: Vector[String] =
        ("Benchmark" +: suite.params.headers :+ "Runs") ++ resultFmts.iterator.flatMap(f => f.header :: "±" :: "error" :: Nil)

      rowBuilder += header
      rowBuilder += Vector.empty

      for (k <- keys) {
        var cells = Vector.empty[String]
        val status = m.getOrElse(k, BMPending)

        cells :+= k.bm.name
        cells ++= suite.params.renderParamsToText(k.param)

        status match {
          case BMPending        => ()
          case BMPreparing      => cells :+= "Preparing..."
          case BMRunning        => cells :+= "Running..."
          case BMDone(Left(e))  => cells :+= ("" + e).takeWhile(_ != '\n')
          case BMDone(Right(r)) =>
            cells :+= Util.addThousandSeps(ValueFmt.Integer toText r.runs)
            for (f <- resultFmts)
              cells ++= Vector(
                Util.addThousandSeps(f.score toText r),
                "±",
                Util.addThousandSeps(f.error toText r))
        }

        rowBuilder += cells
      }

      val preResultColumns = suite.params.headers.length + 1

      def gap(i: Int): String =
        if (i <= preResultColumns || ((i - preResultColumns) % 3) == 0)
          "     "
        else
          " "

      val rows = rowBuilder.result()
      val text = Util.formatTable(rows, gap)
      <.pre(*.resultText, text)
    }

    private def renderResultTable(suite: GuiSuite[P], progress: Progress[P], m: EachBMStatus[P], resultFmts: ResultFmts): VdomElement = {
      val keys = progress.plan.keys

      def header = {
        val th = <.th(*.resultHeader)
        var hs = Vector.empty[VdomTag]
        hs :+= th("Benchmark")
        hs ++= suite.params.headers.map(th(_))
        hs :+= th("Runs")
        hs ++= resultFmts.map(f => <.th(*.resultHeaderScore, resultBlock1, f.header))
        <.tr(hs: _*)
      }

      def runsCell(runs: Int) =
        resultTD(ValueFmt.Integer render runs)

      def rows =
        keys.map { k =>
          val status = m.getOrElse(k, BMPending)
          var hs = Vector.empty[VdomTag]
          hs :+= resultTD(k.bm.name)
          hs ++= suite.params.renderParams(k.param).map(resultTD(_))
          hs ++= (status match {
            case BMPending        => whenBMPending
            case BMPreparing      => whenBMPreparing
            case BMRunning        => whenBMRunning

            case BMDone(Left(err)) =>
              val showError = Callback {
                err.printStackTrace()
              }
              Vector[VdomTag](
                runsCellNone, // Hmmmmm.........
                resultTD(
                  resultBlockAll,
                  <.span(^.color.red, Option(err.toString).filter(_.nonEmpty).getOrElse[String]("ERROR.")),
                  ^.title := "Double-click to print the error to the console",
                  ^.cursor.pointer,
                  ^.onDoubleClick --> showError))

            case BMDone(Right(r)) =>
              runsCell(r.runs) +:
              resultFmts.flatMap(f => Vector(
                resultTD(f.score render r),
                PlusMinusCell,
                resultTD(f.error render r)))
          })
          <.tr(hs: _*)
        }

      <.div(
        <.table(
          *.resultTable,
          <.thead(header),
          <.tbody(rows: _*)))
    }

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
          case BMDone(Right(stats)) => fmt.score.getDouble(stats) getOrElse 0
          case BMDone(Left(_))
               | BMPending
               | BMRunning
               | BMPreparing => -0.1 // 0 puts a thick bar above the axis which looks like a small result
        }
      ).take(bmsToShow).toVector

      val dataset = ScalaDataset(fmt.header, dataPoints)
      val bardata = ScalaBarData(titles, Vector(dataset))
      val props = ReactChart.Props(*.graph, *.graphInner(bardata))

      <.div(*.graphContainer,
        <.div(*.graphHeader, fmt.graphHeader),
        ReactChart.Comp(props))
    }

    private def renderFormatButtons(s: State) =
      <.div(
        *.resultFormatRow,
        "Result format: ",
        ResultFormat.all.toTagMod { f =>
          <.label(
            *.resultFormat,
            <.input.radio(
              ^.checked := (s.resultFormat == f),
              ^.onChange --> $.modState(_.copy(resultFormat = f))),
            f.label)
        }
      )

    private def renderSuiteRunning(p: Props, s: State, r: SuiteRunning): VdomElement = {
      val resultFmts = deriveResultFmts(r.progess, r.bm)

      def abortButton =
        <.button(
          *.abortButton,
          ^.onClick --> r.abortFn.callback,
          "Abort")

      <.div(
        <.div(*.runningRow,
          <.span("Benchmark running..."),
          abortButton),
        renderFormatButtons(s),
        renderResults(s.resultFormat, p.suite, r.progess, r.bm, resultFmts),
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
          <.span(s"Benchmark completed in ${formatTotalTime(r.totalTime)}."),
          resetButton),
        renderFormatButtons(s),
        renderResults(s.resultFormat, p.suite, r.progess, r.bm, resultFmts),
        renderGraph(p.suite, r.progess, r.bm, resultFmts))
    }

    private def renderDesc(e: VdomElement) =
      <.div(*.suiteDesc, e)

    private def deriveResultFmts(progress: Progress[P], m: EachBMStatus[P]): ResultFmts = {
      val keys = progress.plan.keys

      val minAvg =
        keys
          .iterator
          .flatMap(m.get)
          .collect { case BMDone(Right(s)) => s.average }
          .reduceOption(_.min(_))
          .getOrElse(Duration.Zero)

      if (minAvg.toMicros < 1000)
        Vector(ResultFmt.MicrosPerOp, ResultFmt.OpsPerSec)
      else if (minAvg.toMillis < 1000)
        Vector(ResultFmt.MillisPerOp, ResultFmt.OpsPerSec)
      else
        Vector(ResultFmt.SecPerOp, ResultFmt.OpsPerSec)
    }

    def render(p: Props, s: State): VdomElement = {
      val inner: VdomElement = s.status match {
        case r: SuiteRunning => renderSuiteRunning(p, s, r)
        case r: SuiteDone    => renderSuiteDone(p, s, r)
        case SuitePending    => renderSuitePending(p, s)
        case SuiteWillStart  => <.span
      }
      <.div(
        <.h2(*.suiteName, p.suite.name),
        p.suite.desc.whenDefined(renderDesc),
        inner)
    }

    def preMount: Callback = {
      def storeCurrentTitle =
        CallbackTo(document.title) |> Some.apply |> State.oldTitle[P].set

      def setNewTitle =
        $.props.map(p => document.title = p.suite.name)

      storeCurrentTitle >>= ($.modState(_, setNewTitle))
    }

    def shutdown: Callback = {
      def abortIfRunning: SuiteStatus[P] => Callback = {
        case r: SuiteRunning => r.abortFn.callback
        case _: SuiteDone
           | SuitePending
           | SuiteWillStart  => Callback.empty
      }

      def restoreTitle(o: Option[String]): Callback =
        o.fold(Callback.empty)(t => Callback(document.title = t))

      $.state >>= (s =>
        abortIfRunning(s.status) >> restoreTitle(s.oldTitle))
    }
  }

  private val __Comp = {
    // TODO Bloody hack. Really need to accommodate this properly in scalajs-react
    type P = Unit

    val c: Comp[_] =
      ScalaComponent.builder[Props[P]]("SuiteComp")
        .initialStateFromProps(State.init)
        .renderBackend[Backend[P]]
        .componentWillMount(_.backend.preMount)
        // TODO handle suite changes - it's all in state atm
        .componentWillUnmount(_.backend.shutdown)
        .build
    c
  }

  def Comp[P] = __Comp.asInstanceOf[Comp[P]]
}
