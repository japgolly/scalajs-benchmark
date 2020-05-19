package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.gui.Styles.{Editors => *}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import java.util.concurrent.TimeUnit
import org.scalajs.dom.ext.KeyCode
import scala.concurrent.duration.{Duration, FiniteDuration}
import scalacss.ScalaCssReact._

object DurationEditor {

  final case class Props(state      : StateSnapshot[State],
                         style      : Reusable[TagMod],
                         allowEmpty : Boolean,
                         placeholder: String,
                         enabled    : Enabled) {
    @inline def render: VdomElement = Component(this)
  }

  final case class State(text: String) {
    val parsed: Option[Option[FiniteDuration]] = {
      text.trim match {
        case ""             => Some(None)
        case Duration(t, u) => if (t >= 0) Some(Some(FiniteDuration(t, u))) else None
        case _              => None
      }
    }
  }

  object State {
    def init(fd: FiniteDuration): State =
      apply(GuiUtil.showFiniteDuration(fd))

    def init(o: Option[FiniteDuration]): State =
      o.fold(apply(""))(init)
  }

  private def render(p: Props): VdomNode = {
    val s = p.state.value

    val validity =
      s.parsed match {
        case Some(Some(_)) => Valid
        case Some(None)    => Valid.when(p.allowEmpty)
        case None          => Invalid
      }

    def onChange(e: ReactEventFromInput): Callback =
      e.extract(_.target.value)(t => p.state.setState(State(t)))

    def add(n: Int): Callback =
      p.state.setStateOption(s.parsed.flatten.flatMap { d =>
        val newLen = d.length + n
        val unit = if (d.length == 0 || newLen == 0) TimeUnit.SECONDS else d.unit
        val s = State.init(FiniteDuration(newLen, unit))
        s.parsed.map(_ => s) // ignore invalid updates
      })

    def onKeyDown(e: ReactKeyboardEvent): Callback =
      CallbackOption.keyCodeSwitch(e) {
        case KeyCode.Up   => add(1)
        case KeyCode.Down => add(-1)
      }
        .asEventDefault(e)

    <.input.text(
      *.inputText(validity),
      ^.disabled := p.enabled.is(Disabled),
      ^.onChange ==> onChange,
      ^.onKeyDown ==> onKeyDown,
      ^.placeholder := p.placeholder,
      ^.value := s.text,
      p.style)
  }

  implicit val reusabilityState: Reusability[State] = Reusability.derive
  implicit val reusabilityProps: Reusability[Props] = Reusability.derive

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
