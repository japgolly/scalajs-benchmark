package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.engine._
import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle._
import monocle.macros.Lenses
import scala.concurrent.duration._
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import Params.GenState
import Styles.{ResultTable => *}

/**
  * React component that provides the GUI over a [[GuiSuite]].
  */
object SuiteComp {
  type Comp[P] = ReactComponentC.ReqProps[Props[P], State[P], Backend[P], TopNode]

  case class Props[P](suite: GuiSuite[P], options: Options = Options.Default)

  @Lenses
  case class State[A](status: SuiteStatus[A], editors: GenState, disabledBMs: Set[Int])

  object State {
    def at[A](k: PlanKey[A]): Optional[State[A], BMStatus] =
      status ^|-? SuiteStatus.at[A](k)
  }

  type EachBMStatus[P] = Map[PlanKey[P], BMStatus]

  sealed trait SuiteStatus[+P]
  case object SuitePending   extends SuiteStatus[Nothing]
  case object SuiteWillStart extends SuiteStatus[Nothing]
  case class SuiteRunning[P](suite: GuiSuite[P], plan: Plan[P], bm: EachBMStatus[P], abortFn: AbortFn) extends SuiteStatus[P]
  case class SuiteDone   [P](suite: GuiSuite[P], plan: Plan[P], bm: EachBMStatus[P], totalTime: FiniteDuration) extends SuiteStatus[P]

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

  private val PlusMinusCell = <.td("±")

  private def formatTotalTime(fd: FiniteDuration): String =
    StatValueFmt.addThousandSeps("%.2f" format ResultFmt.getUnits(SECONDS)(fd)) + " seconds"

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

            case SuiteFinished(_) =>
              val endTime = System.currentTimeMillis()
              $.modState(State.status.modify { s =>
                val bm = SuiteStatus.running.getOption(s).map(_.bm).getOrElse(Map.empty)
                val time = FiniteDuration(endTime - startTime, MILLISECONDS)
                SuiteDone(suite, plan, bm, time)
              })
          }

          val running = SuiteRunning[P](suite, plan, Map.empty, abort)
          $.modState(State.status set running)
        }.flatten)
      }

    def toggleBM(i: Int): Callback =
      $.modState(State.disabledBMs.modify(s =>
        if (s contains i) s - i else s + i))

    def renderSuitePending(p: Props, s: State): ReactElement = {
      val ev = ExternalVar(s.editors)(updateEditorState)
      val params = p.suite.params

      def bmRow = {
        def bmCell(bm: Benchmark[P], i: Int) =
          <.div(
            <.label(
              <.input(
                ^.`type`    := "checkbox",
                ^.checked   := !s.disabledBMs.contains(i),
                ^.onChange --> toggleBM(i)),
              bm.name))

        <.tr(
          <.th("Benchmarks"),
          <.td(p.suite.suite.bms.iterator.zipWithIndex.map((bmCell _).tupled).toList: _*))
      }

      def paramRow(i: Int) =
        <.tr(
          <.th(params.headers(i)),
          <.td(params.editors(i)(ev)))

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
          <.tbody(
            bmRow,
            paramRows)),
        startButton)
    }

    def renderResultTable(suite: GuiSuite[P], plan: Plan[P], m: EachBMStatus[P]): ReactElement = {
      def header = {
        var hs = Vector.empty[ReactTag]
        hs :+= <.th("Benchmark")
        hs ++= suite.params.headers.map(<.th(_))
        hs ++= resultFmts.map(f => <.th(*.resultHeader, resultBlock1, f.header))
        <.tr(hs: _*)
      }

      def rows =
        plan.keys.map { k =>
          val status = m.getOrElse(k, BMPending)
          var hs = Vector.empty[ReactTag]
          hs :+= <.td(k.bm.name)
          hs ++= suite.params.renderParams(k.param).map(<.td(_))
          hs ++= (status match {
            case BMPending        => Vector.empty :+ <.td(resultBlockAll)
            case BMRunning        => Vector.empty :+ <.td(resultBlockAll, "Running…")
            case BMDone(-\/(err)) => Vector.empty :+ <.td(resultBlockAll, "ERROR")
            case BMDone(\/-(r)) =>
              resultFmts.flatMap(f =>
                Vector(
                  <.td(f.score render r),
                  PlusMinusCell,
                  <.td(f.error render r)))
          })
          <.tr(hs: _*)
        }

      def graph = {
        import ReactChart._
        val fmt = resultFmts.head
        val bmsToShow = m.size max 1

        val titles = plan.keys.iterator
            .map(k => s"${k.bm.name} @ ${k.param}")
            .take(bmsToShow)
            .toVector

        val dataPoints = plan.keys.iterator.map[Chart.Value](k =>
          m.getOrElse(k, BMPending) match {
            case BMDone(\/-(stats)) => fmt.score.getDouble(stats) getOrElse 0
            case BMDone(-\/(_))
               | BMPending
               | BMRunning => -0.1 // 0 puts a thick bar above the axis which looks like a small result
          }
        ).take(bmsToShow).toVector

        val dataset = ScalaDataset(fmt.header, dataPoints)
        val bardata = ScalaBarData(titles, Vector(dataset))
        val props = ReactChart.Props(*.graphOuter, *.graphInner(bardata))
        ReactChart.Comp(props)
      }

      <.div(
        <.table(
          *.table,
          <.thead(header),
          <.tbody(rows: _*)),
        <.div(
          graph))
    }

    def renderSuiteRunning(p: Props, s: State, r: SuiteRunning): ReactElement =
      <.div(
        renderResultTable(p.suite, r.plan, r.bm),
        <.button("Abort", ^.onClick --> r.abortFn.callback))

    def renderSuiteDone(p: Props, s: State, r: SuiteDone): ReactElement =
      <.div(
        renderResultTable(p.suite, r.plan, r.bm),
        <.div(s"Benchmark completed in ${formatTotalTime(r.totalTime)}."))

    def render(p: Props, s: State): ReactElement = {
      val inner: ReactElement = s.status match {
        case r: SuiteRunning => renderSuiteRunning(p, s, r)
        case r: SuiteDone    => renderSuiteDone(p, s, r)
        case SuitePending    => renderSuitePending(p, s)
        case SuiteWillStart  => <.span
      }
      <.div(
        <.h3(p.suite.name),
        inner)
    }
  }

  private val __Comp = {
    // TODO Bloody hack. Really need to accommodate this properly in scalajs-react
    type P = Unit
    val c: Comp[_] =
      ReactComponentB[Props[P]]("SuiteComp")
        .initialState_P[State[P]](p => State[P](SuitePending, p.suite.params.initialState, Set.empty))
        .renderBackend[Backend[P]]
        // TODO handle suite changes - it's all in state atm
        .build
    c
  }

  def Comp[P] = __Comp.asInstanceOf[Comp[P]]
}
