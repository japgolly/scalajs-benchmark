package whatever

import japgolly.scalajs.react._, vdom.prefix_<^._, MonocleReact._
import scalajs.js
import org.scalajs.dom
import monocle.macros._
import monocle._

import Benchy._

object Main extends js.JSApp {
  def main(): Unit = {

    val s = IntSet_X.suite
    //    Benchy.runSuite(s)

//        runToConsole(s)

    val tgt = dom.document.getElementById("body")
    ReactDOM.render(Comp(), tgt)
  }

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

  class Backend($: BackendScope[Props, State]) {
    val s = IntSet_X.suite

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
          def rows = s.iterator { (key, b, p) =>
            val y = m.getOrElse(key, Nope)
            val x: TagMod = y match {
              case Nope => EmptyTag
              case Running => "Running…"
              case Done(r) => <.pre(r.toString)
            }

            <.tr(
              <.td(key.bm(s).name),
              <.td(key.param(s).toString),
              <.td(x))
          }.toList

          <.div(
            <.table(<.tbody(rows: _*)),
            <.button("Abort", ^.onClick --> Callback(TEMP_HACK_ABORT.run()))

          )


      }
      <.div(
        <.h1(s.name),
        body)
    }
  }

  val Comp = ReactComponentB[Props]("")
    .initialState[State](Mada)
    .renderBackend[Backend]
    .buildU
}
