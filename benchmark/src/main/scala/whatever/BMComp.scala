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
import NotMyProb._

import scalaz.{\/-, -\/}

object BMComp {

  def newObj[T <: js.Object]: T =
    js.Object().asInstanceOf[T]

  class Suite2[P](val suite: Suite.WithParam[P], val paramFmt: Params[P])
  object Suite2 {
    def apply[P](suite: Suite.WithParam[P])(paramFmt: Params[P]) =
      new Suite2(suite, paramFmt)
  }

  sealed trait BMState
  case object Nope extends BMState
  case object Running extends BMState
  case class Done(result: RunResult) extends BMState

  sealed trait SuiteStatus[+P]
  case object Mada extends SuiteStatus[Nothing]
  case class Running[P](suite: Suite.WithParam[P], m: Map[BMKey, BMState]) extends SuiteStatus[P]

  object SuiteStatus {
    def running[P]: Prism[SuiteStatus[P], Running[P]] =
      Prism[SuiteStatus[P], Running[P]] { case r: Running[P] => Some(r); case _ => None }(s => s)

    def runningAt[P](k: BMKey): Lens[Running[P], BMState] =
      Lens[Running[P], BMState](_.m.getOrElse(k, Nope))(s => r => Running(r.suite, r.m.updated(k, s)))

    def at[P](k: BMKey): Optional[SuiteStatus[P], BMState] =
      running ^|-> runningAt(k)
  }

  @Lenses
  case class State[A](status: SuiteStatus[A], ep: GenState)
  object State {
    def at[A](k: BMKey): Optional[State[A], BMState] =
      status ^|-? SuiteStatus.at[A](k)
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
//      implicit
      val s = s2.suite

      val resultFmts = Vector(ResultFmt.MicrosPerOp, ResultFmt.OpsPerSec)
      val resultBlock1 = ^.colSpan := 3
      val resultBlockAll = ^.colSpan := (3 * resultFmts.length)

      val header = {
        var hs = Vector.empty[ReactTag]
        hs :+= <.th("Benchmark")
        hs ++= s2.paramFmt.paramDefs.map(f => <.th(f.param.header))
        hs ++= resultFmts.map(f =>
          <.th(*.resultHeader, resultBlock1, f.header))
        <.tr(hs: _*)
      }

      var TEMP_HACK_ABORT: AbortFn = _

      def start(gs: GenState): Callback = {

        def run(sss: Suite.WithParam[P]) =
          $.modState(State.status.set(Running(sss, Map.empty)), Callback(
            TEMP_HACK_ABORT = runSuiteAsync(sss) {
              case SuiteStarting(p) => Callback.empty
              case BenchmarkStarting(p, k) => $.modState(State.at(k).set(Running))
              case BenchmarkFinished(p, k, r) => $.modState(State.at(k).set(Done(r)))
              case SuiteFinished(p) => Callback.log("bye")
            }
          ))

        s2.paramFmt.forState(gs) match {
          case \/-(ps) => run(s.withParams(ps))
          case -\/(e) => Callback.alert(s"Error in ${e.param.header} editor.")
        }
      }

      def render(state: State[P]) = {
        val body: ReactTag = state.status match {
          case Mada =>

            val ev = ExternalVar(state.ep)(n => $.modState(State.ep set n))

            <.div(
              <.div(
                "Params",
                <.table(<.tbody(
                  TagMod(s2.paramFmt.paramDefs.map(p =>
                    <.tr(
                      <.th(p.param.header), <.td(p.editor(ev))
                    )
                  ): _*)
                ))
              ),
              <.button("Start", ^.onClick --> start(ev.value))
            )

          case Running(s3, m) =>

            def rows = {
              s3.keys.map { key =>
                val b = key bm s3
                val p = key param s3
                val y = m.getOrElse(key, Nope)

                var hs = Vector.empty[ReactTag]
                hs :+= <.td(b.name)
                hs ++= s2.paramFmt.paramDefs.map(f => <.td(f.param renderValue p))

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
              case Running(s3, r) =>
                implicit def ssss = s3

                import ReactChart._

                val f = resultFmts.head

                val n = r.size max 1

                val bd =
                  ScalaBarData(
                    s3.keys.iterator.map(k => s"${k.bm.name} @ ${k.param.toString}").take(n).toVector,
                    Vector(*.styleDataset(ScalaDataset(
                      f.header,
                      s3.keys.iterator.map[Chart.Value](k =>
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
        .initialState_P[State[P]](p => State[P](Mada, p.paramFmt.initState))
        .renderBackend[Backend[P]]
        // TODO handle suite changes - it's all in state atm
        .build
    c
  }

  def Comp[P] = __Comp.asInstanceOf[Comp[P]]
}
