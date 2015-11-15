package whatever

import japgolly.scalajs.react._, vdom.prefix_<^._, MonocleReact._
import whatever.ReactChart.ScalaDataset
import whatever.chartjs.Chart
import scala.concurrent.duration._
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit._
import scalajs.js
import org.scalajs.dom
import monocle.macros._
import monocle._
import scalacss.Defaults._
import scalacss.ScalaCssReact._

import Benchy._

object Main extends js.JSApp {
  def main(): Unit = {

    val s = IntSet_X.suite
    //    Benchy.runSuite(s)

//        runToConsole(s)

    chartjs.Chart.defaults.global.animationSteps = 20

    // Ensure benchmarks don't start before chart animation finishes
    Benchy.minBmDelay = {
      val chartTimeSec = chartjs.Chart.defaults.global.animationSteps / 60.0
      val delaySec = chartTimeSec * 1.2
      val delayMicro = delaySec * 1000000.0
      delayMicro.toInt.micros
    }

    Styles.addToDocument()
    val tgt = dom.document.getElementById("body")
    ReactDOM.render(Comp(), tgt)
  }

  def newObj[T <: js.Object]: T =
    js.Object().asInstanceOf[T]

  type Props = Unit

  sealed trait BMState
  case object Nope extends BMState
  case object Running extends BMState
  case class Done(result: RunResult) extends BMState

  sealed trait State
  case object Mada extends State
  case class Running(m: Map[BMKey, BMState]) extends State

  object State {
    val running: Prism[State, Running] =
      Prism[State, Running] { case r: Running => Some(r); case _ => None }(s => s)

    def runningAt(k: BMKey): Lens[Running, BMState] =
      Lens[Running, BMState](_.m.getOrElse(k, Nope))(s => r => Running(r.m.updated(k, s)))

    def at(k: BMKey): Optional[State, BMState] =
      running ^|-> runningAt(k)
  }

  def abbrev(t: TimeUnit): String =
    t match {
      case TimeUnit.NANOSECONDS  => "ns"
      case TimeUnit.MICROSECONDS => "μs"
      case TimeUnit.MILLISECONDS => "ms"
      case TimeUnit.SECONDS      => "s"
      case TimeUnit.MINUTES      => "m"
      case TimeUnit.HOURS        => "d"
      case TimeUnit.DAYS         => "hr"
    }
  def mkDouble(t: TimeUnit): FiniteDuration => Double =
    t match {
      case TimeUnit.NANOSECONDS  => _.toNanos.toDouble
      case TimeUnit.MICROSECONDS => _.toNanos.toDouble / 1000.0
      case TimeUnit.MILLISECONDS => _.toNanos.toDouble / 1000000.0
      case TimeUnit.SECONDS      => _.toMicros.toDouble / 1000000.0
      case TimeUnit.MINUTES      => _.toMillis.toDouble / 60000.0
      case TimeUnit.HOURS        => _.toMillis.toDouble / 3660000.0
      case TimeUnit.DAYS         => _.toSeconds.toDouble / (3660 * 24)
    }

  trait ValueFmt {
    def render(s: RunStats): ReactElement
    def asDouble(s: RunStats): Option[Double]
  }

  case class ResultFmt(header: String,
                       fmtScore: ValueFmt,
                       fmtMoE: ValueFmt)

  object ResultFmt {
    def fmtDurToDbl(fmtF: FiniteDuration => Double, dp: Int): ValueFmt =
      new ValueFmt {
        val fmt = if (dp <= 0) "%0f" else s"%0.${dp}f"

        def fmtD(avgOpDuration: Duration): Option[Double] =
          avgOpDuration match {
            case f: FiniteDuration => Some(fmtF(f))
            case _ => None
          }

        def fmtS(od: Option[Double]): String =
          od.fold("∞")(scoreToString)

        def scoreToString(d: Double) = fmt format d

        override def asDouble(s: RunStats) =
          fmtD(s.average)

        override def render(s: RunStats): ReactElement =
          <.div(Styles.ResultTable.numericResult, fmtS(asDouble(s)))
      }

    val fmtError: ValueFmt = new ValueFmt {
      override def asDouble(s: RunStats) = None
      override def render(s: RunStats): ReactElement = <.div("?")
    }

    def OpsPerT(t: TimeUnit, dp: Int): ResultFmt = {
      val one = FiniteDuration(1, t)
      val hdr = "ops/" + abbrev(t)
      val fmtScore = fmtDurToDbl(one / _, dp)
      ResultFmt(hdr, fmtScore, fmtError)
    }

    def TPerOp(t: TimeUnit, dp: Int): ResultFmt = {
      val hdr = abbrev(t) + "/op"
      val fmtScore = fmtDurToDbl(mkDouble(t), dp)
      ResultFmt(hdr, fmtScore, fmtError)
    }

    val OpsPerSec   = OpsPerT(TimeUnit.SECONDS, 3)
    val MillisPerOp = TPerOp(TimeUnit.MILLISECONDS, 3)
    val MicrosPerOp = TPerOp(TimeUnit.MICROSECONDS, 3)
  }


  class Backend($: BackendScope[Props, State]) {
    import Styles.{ResultTable => *}
    implicit val s = IntSet_X.suite

    val resultFmts = Vector(ResultFmt.MicrosPerOp, ResultFmt.OpsPerSec)
    val resultBlock1 = ^.colSpan := 3
    val resultBlockAll = ^.colSpan := (3 * resultFmts.length)

    var TEMP_HACK_ABORT: AbortFn = _

    def start: Callback =
      $.setState(Running(Map.empty), Callback(
        TEMP_HACK_ABORT = runSuiteAsync(s) {
          case SuiteStarting    (p)       => Callback.empty
          case BenchmarkStarting(p, k)    => $.modState(State.at(k).set(Running))
          case BenchmarkFinished(p, k, r) => $.modState(State.at(k).set(Done(r)))
          case SuiteFinished    (p)       => Callback.log("bye")
        }
      ))

    def render(state: State) = {
      val body: ReactTag = state match {
        case Mada => <.button("Start", ^.onClick --> start)
        case Running(m) =>

          def header =
            <.tr(
              <.th("Benchmark"),
              <.th("Params"))(
              resultFmts.map(f =>
                <.th(
                  *.resultHeader,
                  resultBlock1,
                  f.header)
              ): _*)

          def rows = s.keys.map { key =>
            val b = key bm s
            val p = key param s
            val y = m.getOrElse(key, Nope)
            val x: TagMod = y match {
              case Nope    => <.td(resultBlockAll)
              case Running => <.td(resultBlockAll, "Running…")
              case Done(Left(err)) => ??? // ////////////////////////////////////////////////
              case Done(Right(r)) =>
                //<.pre(r.toString)
                resultFmts.map(f => TagMod(
                  <.td(f.fmtScore render r),
                  <.td("±"),
                  <.td(f.fmtMoE render r))
                ).reduce(_ + _)
            }

            <.tr(
              <.td(b.name),
              <.td(p.toString),
              x)
          }

          def graph: TagMod = state match {
            case Running(r) =>

              import ReactChart._

              val f = resultFmts.head

              val n = r.size max 1

              val bd =
              ScalaBarData(
                s.keys.iterator.map(k => s"${k.bm.name} @ ${k.param.toString}").take(n).toVector,
                Vector(*.styleDataset(ScalaDataset(
                  f.header,
                  s.keys.iterator.map[Chart.Value](k =>
                    r.getOrElse(k, Nope) match {
                      case Done(Right(rr)) => f.fmtScore.asDouble(rr) getOrElse 0
                      case Done(Left(_)) | Nope | Running => 0
                    }
                  ).take(n).toVector
                )))
              )

              val p = Props(*.graph, bd)
              ReactChart.Comp(p)

//              import whatever.chartjs._
//              val bd = newObj[BarData]
//              val ds1 = newObj[Dataset]
//
//              bd.labels = js.Array() //"Hello","b","c")
//              ds1.label = "hehe"
//              ds1.data = js.Array() //23,14,50)
//            val ds2 = newObj[Dataset]
//              ds2.label = "hoho"
//              ds2.data = js.Array() //32,7,13)
//              bd.datasets = js.Array(ds1, ds2)
//              val cp = Charty.Props(
//                TagMod(^.width := 400, ^.height := 400),
//                bd)

            case Mada => EmptyTag
          }

          <.div(
            <.table(
              *.table,
              <.thead(header),
              <.tbody(rows: _*)),
            <.button("Abort", ^.onClick --> Callback(TEMP_HACK_ABORT.run())),
            graph
          )


      }
      <.div(
        <.h1(s.name),
        body
        /*
        ,
        Charty.Comp(cp),
        <.button("more", ^.onClick --> Callback {
//          ds1.data push (ds1.## % 50)
//          ds2.data push (ds2.## % 50)
          ds2.data(0) = 90
          println(ds2.data)
          $.forceUpdate.runNow()
        })
        */
      )
    }

    /*
    import whatever.chartjs._
    val bd = newObj[BarData]
    bd.labels = js.Array() //"Hello","b","c")
    val ds1 = newObj[Dataset]
    ds1.label = "hehe"
    ds1.data = js.Array() //23,14,50)
    val ds2 = newObj[Dataset]
    ds2.label = "hoho"
    ds2.data = js.Array()//32,7,13)
    bd.datasets = js.Array(ds1, ds2)
    val cp = Charty.Props(
      TagMod(^.width := 400, ^.height := 400),
      bd)
*/
  }

  val Comp = ReactComponentB[Props]("")
    .initialState[State](Mada)
    .renderBackend[Backend]
    .buildU

  object Charty {

  }

}
