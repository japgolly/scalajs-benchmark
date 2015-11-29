package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.engine._
import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle._
import monocle.macros.Lenses
import org.scalajs.dom.document
import scala.concurrent.duration._
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import GuiParams.GenState
import Styles.{Suite => *}

/**
  * React component that provides the GUI over a [[GuiSuite]].
  */
object SuiteComp {
  type Comp[P] = ReactComponentC.ReqProps[Props[P], State[P], Backend[P], TopNode]

  case class Props[P](suite: GuiSuite[P], options: Options = Options.Default)

  @Lenses
  case class State[A](status     : SuiteStatus[A],
                      editors    : GenState,
                      disabledBMs: Set[Int],
                      oldTitle   : Option[String])

  object State {
    def at[A](k: PlanKey[A]): Optional[State[A], BMStatus] =
      status ^|-? SuiteStatus.at[A](k)

    def initDisabledBMs(bms: Vector[Benchmark[Nothing]]): Set[Int] =
      bms.iterator.zipWithIndex.filter(_._1.isDisabledByDefault).map(_._2).toSet

    def init[P](p: Props[P]): State[P] =
      State[P](SuitePending, p.suite.params.initialState, initDisabledBMs(p.suite.suite.bms), None)
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
  case object BMRunning extends BMStatus
  case class BMDone(result: Result) extends BMStatus

  private val resultFmts = Vector(ResultFmt.MicrosPerOp, ResultFmt.OpsPerSec)
  private val resultBlock1 = ^.colSpan := 3
  private val resultBlockAll = ^.colSpan := (3 * resultFmts.length)

  private val resultTD = <.td(*.resultData)

  private val PlusMinusCell = resultTD("±")

  private val runsCellNone = resultTD
  private val whenBMPending = Vector[ReactTag](runsCellNone, resultTD(resultBlockAll))
  private val whenBMRunning = Vector[ReactTag](runsCellNone, resultTD(resultBlockAll, "Running…"))

  private def formatTotalTime(fd: FiniteDuration): String =
    ValueFmt.addThousandSeps("%.2f" format ResultFmt.getUnits(SECONDS)(fd)) + " seconds"

  final class Backend[P]($: BackendScope[SuiteComp.Props[P], SuiteComp.State[P]]) {
    type Props        = SuiteComp.Props[P]
    type State        = SuiteComp.State[P]
    type SuiteRunning = SuiteComp.SuiteRunning[P]
    type SuiteDone    = SuiteComp.SuiteDone[P]

    val guiSuiteBMs = GuiSuite.suite[P] ^|-> Suite.bms

    val updateEditorState: GenState => Callback =
      s => $.modState(State.editors set s)

    def start(suite: GuiSuite[P], options: Options, ps: Vector[P]): Callback =
      Callback.byName {

        // Prepare to start
        val startTime = System.currentTimeMillis()
        val plan = Plan(suite.suite, ps)
        $.modState(State.status set SuiteWillStart, CallbackTo {

          // Actually start
          val abort = Engine.run(plan, options) {

            case BenchmarkStarting(_, k) =>
              $.modState(State.at(k) set BMRunning)

            case BenchmarkFinished(_, k, r) =>
              $.modState(State.at(k) set BMDone(r))

            case SuiteStarting(_) =>
              Callback.empty

            case SuiteFinished(progress) =>
              val endTime = System.currentTimeMillis()
              $.modState(State.status.modify { s =>
                val bm = SuiteStatus.running.getOption(s).map(_.bm).getOrElse(Map.empty)
                val time = FiniteDuration(endTime - startTime, MILLISECONDS)
                SuiteDone(suite, progress, bm, time)
              })
          }

          val running = SuiteRunning[P](suite, Progress(plan, 0), Map.empty, abort)
          $.modState(State.status set running)
        }.flatten)
      }

    def toggleBM(i: Int): Callback =
      $.modState(State.disabledBMs.modify(s =>
        if (s contains i) s - i else s + i))

    def makeSoleBM(i: Int): Callback =
      $.props >>= (p =>
        $.modState(State.disabledBMs.set(
          p.suite.suite.bms.indices.toSet - i)))

    def renderSuitePending(p: Props, s: State): ReactElement = {
      val ev = ExternalVar(s.editors)(updateEditorState)
      val params = p.suite.params
      val th = <.th(*.settingsTableHeader)
      val td = <.td(*.settingsTableData)

      def bmRow = {
        def bmCell(bm: Benchmark[P], i: Int) =
          <.label(
            *.settingsTableBm,
            ^.onDoubleClick --> makeSoleBM(i),
            <.input(
              ^.`type`         := "checkbox",
              ^.checked        := !s.disabledBMs.contains(i),
              ^.onChange      --> toggleBM(i)),
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
          EmptyTag
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
          start(s2, p.options, ps)
        }
      }

      def startButton =
        <.button(
          *.startButton,
          ^.disabled := onStart.isEmpty,
          ^.onClick -->? onStart,
          "Start")

      <.div(
        <.table(
          *.settingsTable,
          <.tbody(
            bmRow,
            paramRows)),
        startButton)
    }

    def renderResultTable(suite: GuiSuite[P], progress: Progress[P], m: EachBMStatus[P]): ReactElement = {
      val keys = progress.plan.keys

      def header = {
        val th = <.th(*.resultHeader)
        var hs = Vector.empty[ReactTag]
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
          var hs = Vector.empty[ReactTag]
          hs :+= resultTD(k.bm.name)
          hs ++= suite.params.renderParams(k.param).map(resultTD(_))
          hs ++= (status match {
            case BMPending        => whenBMPending
            case BMRunning        => whenBMRunning
            case BMDone(-\/(err)) =>
              Vector[ReactTag](
                runsCellNone, // Hmmmmm.........
                resultTD(resultBlockAll, "ERROR", ^.onDoubleClick --> Callback{throw err; ()}))
            case BMDone(\/-(r)) =>
              runsCell(r.runs) +:
              resultFmts.flatMap(f => Vector(
                resultTD(f.score render r),
                PlusMinusCell,
                resultTD(f.error render r)))
          })
          <.tr(hs: _*)
        }

      def graph = {
        import ReactChart._
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
            case BMDone(\/-(stats)) => fmt.score.getDouble(stats) getOrElse 0
            case BMDone(-\/(_))
               | BMPending
               | BMRunning => -0.1 // 0 puts a thick bar above the axis which looks like a small result
          }
        ).take(bmsToShow).toVector

        val dataset = ScalaDataset(fmt.header, dataPoints)
        val bardata = ScalaBarData(titles, Vector(dataset))
        val props = ReactChart.Props(*.graph, *.graphInner(bardata))

        <.div(*.graphContainer,
          <.div(*.graphHeader, fmt.graphHeader),
          ReactChart.Comp(props))
      }

      <.div(
        <.table(
          *.resultTable,
          <.thead(header),
          <.tbody(rows: _*)),
        graph)
    }

    def renderSuiteRunning(p: Props, s: State, r: SuiteRunning): ReactElement = {
      def abortButton =
        <.button(
          *.abortButton,
          ^.onClick --> r.abortFn.callback,
          "Abort")

      <.div(
        <.div(*.runningRow,
          <.span("Benchmark running..."),
          abortButton),
        renderResultTable(p.suite, r.progess, r.bm))
    }

    def renderSuiteDone(p: Props, s: State, r: SuiteDone): ReactElement = {
      def resetButton =
        <.button(
          *.resetButton,
          ^.onClick --> $.modState(State.status set SuitePending),
          "Reset")

      <.div(
        <.div(*.doneRow,
          <.span(s"Benchmark completed in ${formatTotalTime(r.totalTime)}."),
          resetButton),
        renderResultTable(p.suite, r.progess, r.bm))
    }

    def renderDesc(e: ReactElement) =
      <.div(*.suiteDesc, e)

    def render(p: Props, s: State): ReactElement = {
      val inner: ReactElement = s.status match {
        case r: SuiteRunning => renderSuiteRunning(p, s, r)
        case r: SuiteDone    => renderSuiteDone(p, s, r)
        case SuitePending    => renderSuitePending(p, s)
        case SuiteWillStart  => <.span
      }
      <.div(
        <.h2(*.suiteName, p.suite.name),
        p.suite.desc.map(renderDesc),
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
      ReactComponentB[Props[P]]("SuiteComp")
        .initialState_P(State.init)
        .renderBackend[Backend[P]]
        .componentWillMount(_.backend.preMount)
        // TODO handle suite changes - it's all in state atm
        .componentWillUnmount(_.backend.shutdown)
        .build
    c
  }

  def Comp[P] = __Comp.asInstanceOf[Comp[P]]
}
