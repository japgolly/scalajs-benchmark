package whatever

import japgolly.scalajs.react._, vdom.prefix_<^._, MonocleReact._
import japgolly.scalajs.react.extra._
import whatever.chartjs.Chart
import scalajs.js
import monocle._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import Benchy._
import Formaty._

object BMComp {

  def newObj[T <: js.Object]: T =
    js.Object().asInstanceOf[T]

  class Suite2[P](val suite: Suite.WithParam[P], val paramFmt: FmtParams[P])
  object Suite2 {
    def apply[P](suite: Suite.WithParam[P])(paramFmt: FmtParams[P]) =
      new Suite2(suite, paramFmt)
  }

  sealed trait BMState
  case object Nope extends BMState
  case object Running extends BMState
  case class Done(result: RunResult) extends BMState

  sealed trait SuiteStatus
  case object Mada extends SuiteStatus
  case class Running(m: Map[BMKey, BMState]) extends SuiteStatus

  object SuiteStatus {
    val running: Prism[SuiteStatus, Running] =
      Prism[SuiteStatus, Running] { case r: Running => Some(r); case _ => None }(s => s)

    def runningAt(k: BMKey): Lens[Running, BMState] =
      Lens[Running, BMState](_.m.getOrElse(k, Nope))(s => r => Running(r.m.updated(k, s)))

    def at(k: BMKey): Optional[SuiteStatus, BMState] =
      running ^|-> runningAt(k)
  }

  @Lenses
  case class State[A](status: SuiteStatus)
  object State {
    def at[A](k: BMKey): Optional[State[A], BMState] =
      status ^|-? SuiteStatus.at(k)
  }


  type Props[P] = Suite2[P]

  class Backend[P]($: BackendScope[Props[P], State[P]]) {
    import Styles.{ResultTable => *}

    implicit def suiteReuse = Reusability.byRef[Suite2[P]]
    val arcane = Px.bs($).propsA.map(new Arcane(_))

    import ReactChart.RGB
    import ReactChart.ColourByValue
    import ReactChart.ColourByValue.scaleFn
    val graphFX = Some(ColourByValue(
      fill = Some(scaleFn(RGB(32,255,32), RGB(255,32,32))),
      stroke = Some(scaleFn(RGB(0,92,0), RGB(92,0,0)))))


    class Arcane(val s2: Suite2[P]) {
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
        $.modState(State.status set Running(Map.empty), Callback(
          TEMP_HACK_ABORT = runSuiteAsync(s) {
            case SuiteStarting(p) => Callback.empty
            case BenchmarkStarting(p, k) => $.modState(State.at(k).set(Running))
            case BenchmarkFinished(p, k, r) => $.modState(State.at(k).set(Done(r)))
            case SuiteFinished(p) => Callback.log("bye")
          }
        ))

      def render(state: State[P]) = {
        val body: ReactTag = state.status match {
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

            def graph: TagMod = state.status match {
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

                val p = Props(*.graph, bd, fx = graphFX)
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

    def render(s: State[P]) = arcane.value().render(s)
  }

  type Comp[P] = ReactComponentC.ReqProps[Props[P], State[P], Backend[P], TopNode]
  private val __Comp = {
    // TODO Bloody hack. Really need to accommodate this properly in scalajs-react
    type P = Unit
    val c: Comp[_] =
      ReactComponentB[Props[P]]("")
        .initialState[State[P]](State[P](Mada))
        .renderBackend[Backend[P]]
        // TODO when suite changes, abort current & wipe state
        .build
    c
  }

  def Comp[P] = __Comp.asInstanceOf[Comp[P]]
}
