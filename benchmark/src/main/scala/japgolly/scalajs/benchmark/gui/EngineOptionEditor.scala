package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.benchmark.gui.Styles.{Editors => *}
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._

object EngineOptionEditor {

  final case class Style(row: VdomTag, key: VdomTag, value: VdomTag) {
    def renderKV(k: String)(v: VdomNode) = {
      row(key(k), value(v))
    }
  }

  final case class Props(state  : StateSnapshot[State],
                         enabled: Enabled,
                         style  : Reusable[Style]) {

    @inline def render: VdomElement = Component(this)
  }

  @Lenses
  final case class State(warmupCnt: IntEditor.State,
                         warmupDur: DurationEditor.State,
                         mainCnt  : IntEditor.State,
                         mainDur  : DurationEditor.State) {

    val parsed: Option[EngineOptions => EngineOptions] =
      for {
        warmupCnt <- warmupCnt.parsed
        warmupDur <- warmupDur.parsed
        mainCnt   <- mainCnt.parsed
        mainDur   <- mainDur.parsed.flatten
      } yield (_: EngineOptions).copy(
        warmupIterations    = warmupCnt,
        warmupIterationTime = warmupDur,
        iterations          = mainCnt,
        iterationTime       = mainDur,
      )
  }

  object State {
    def init(e: EngineOptions): State =
      apply(
        warmupCnt = IntEditor.State.init(e.warmupIterations),
        mainCnt   = IntEditor.State.init(e.iterations),
        warmupDur = DurationEditor.State.init(e.warmupIterationTime),
        mainDur   = DurationEditor.State.init(e.iterationTime),
      )
  }

  private val cntStyle = Reusable.byRef(*.engineOptionCnt: TagMod)
  private val durStyle = Reusable.byRef(*.engineOptionDur: TagMod)

  final class Backend($: BackendScope[Props, Unit]) {
    private val ssWarmupCnt = StateSnapshot.withReuse.zoomL(State.warmupCnt).prepareViaProps($)(_.state)
    private val ssWarmupDur = StateSnapshot.withReuse.zoomL(State.warmupDur).prepareViaProps($)(_.state)
    private val ssMainCnt   = StateSnapshot.withReuse.zoomL(State.mainCnt).prepareViaProps($)(_.state)
    private val ssMainDur   = StateSnapshot.withReuse.zoomL(State.mainDur).prepareViaProps($)(_.state)

    def render(p: Props): VdomNode = {
      val style = p.style.value
      val ss = p.state

      def row(cnt: IntEditor.Props, dur: DurationEditor.Props) =
        <.div(cnt.render, " x ", dur.render)

      val warmups =
        style.renderKV("Warmup Iterations") {

          val cnt = IntEditor.Props(
            state    = ssWarmupCnt(p.state.value),
            validate = IntEditor.Validator.atLeast0,
            style    = cntStyle,
            enabled  = p.enabled,
          )

          val dur = DurationEditor.Props(
            state       = ssWarmupDur(p.state.value),
            style       = durStyle,
            allowEmpty  = true,
            placeholder = ss.value.mainDur.parsed.flatten.fold("")(GuiUtil.showFiniteDuration),
            enabled     = p.enabled,
          )

          row(cnt, dur)
        }

      val iterations =
        style.renderKV("Iterations") {

          val cnt = IntEditor.Props(
            state    = ssMainCnt(p.state.value),
            validate = IntEditor.Validator.atLeast1,
            style    = cntStyle,
            enabled  = p.enabled,
          )

          val dur = DurationEditor.Props(
            state       = ssMainDur(p.state.value),
            style       = durStyle,
            allowEmpty  = false,
            placeholder = "",
            enabled     = p.enabled,
          )

          row(cnt, dur)
        }

      React.Fragment(warmups, iterations)
    }
  }

  implicit val reusabilityState: Reusability[State] = Reusability.derive
  implicit val reusabilityProps: Reusability[Props] = Reusability.derive

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}