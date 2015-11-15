package whatever

import japgolly.scalajs.react._, vdom.prefix_<^._, MonocleReact._
import japgolly.scalajs.react.extra._
import whatever.chartjs.Chart
import scalajs.js
import monocle._
import scalacss.ScalaCssReact._
import Benchy._
import Formaty._

object BMComp {

  def newObj[T <: js.Object]: T =
    js.Object().asInstanceOf[T]

  trait Suite2 {
    type Param
    val suite: Suite.WithParam[Param]
    val paramFmt: FmtParams[Param]
  }
  object Suite2 {
    type WithParam[A] = Suite2 {type Param = A}
    def apply[A](s: Suite.WithParam[A])(p: FmtParams[A]): WithParam[A] =
      new Suite2 {
        override type Param = A
        override val suite = s
        override val paramFmt = p
      }
  }

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

  type Props = Suite2

  class Backend($: BackendScope[Props, State]) {
    import Styles.{ResultTable => *}

    implicit def suiteReuse = Reusability.byRef[Suite2]
    val arcane = Px.bs($).propsA.map(new Arcane(_))

    class Arcane(val s2: Suite2) {
      implicit val s = s2.suite

      val resultFmts = Vector(ResultFmt.MicrosPerOp, ResultFmt.OpsPerSec)
      val resultBlock1 = ^.colSpan := 3
      val resultBlockAll = ^.colSpan := (3 * resultFmts.length)

      val header = {
        var hs = Vector.empty[ReactTag]
        hs :+= <.th("Benchmark")
        hs ++= s2.paramFmt.map(f => <.th(f.header))
        hs ++= resultFmts.map(f =>
          <.th(*.resultHeader, resultBlock1, f.header))
        <.tr(hs: _*)
      }

      var TEMP_HACK_ABORT: AbortFn = _

      def start: Callback =
        $.setState(Running(Map.empty), Callback(
          TEMP_HACK_ABORT = runSuiteAsync(s) {
            case SuiteStarting(p) => Callback.empty
            case BenchmarkStarting(p, k) => $.modState(State.at(k).set(Running))
            case BenchmarkFinished(p, k, r) => $.modState(State.at(k).set(Done(r)))
            case SuiteFinished(p) => Callback.log("bye")
          }
        ))

      def render(state: State) = {
        val body: ReactTag = state match {
          case Mada => <.button("Start", ^.onClick --> start)
          case Running(m) =>

            def rows = {
              s.keys.map { key =>
                val b = key bm s
                val p = key param s
                val y = m.getOrElse(key, Nope)

                var hs = Vector.empty[ReactTag]
                hs :+= <.td(b.name)
                hs ++= s2.paramFmt.map(f => <.td(f render p))

                hs ++= (y match {
                  case Nope    => Vector.empty :+ <.td(resultBlockAll)
                  case Running => Vector.empty :+ <.td(resultBlockAll, "Running…")
                  case Done(Left(err)) => ??? // ////////////////////////////////////////////////
                  case Done(Right(r)) =>
                    //<.pre(r.toString)
                    resultFmts.flatMap(f =>
                      Vector(
                      <.td(f.fmtScore render r),
                      <.td("±"),
                      <.td(f.fmtMoE render r)))
                })

                <.tr(hs: _*)
              }
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
                          case Done(Left(_)) | Nope | Running => -0.1
                        }
                      ).take(n).toVector
                    )))
                  )

                val p = Props(*.graph, bd)
                ReactChart.Comp(p)

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
        )
      }
    }

    def render(s: State) = arcane.value().render(s)
  }

  val Comp = ReactComponentB[Props]("")
    .initialState[State](Mada)
    .renderBackend[Backend]
    // TODO when suite changes, abort current & wipe state
    .build
}
