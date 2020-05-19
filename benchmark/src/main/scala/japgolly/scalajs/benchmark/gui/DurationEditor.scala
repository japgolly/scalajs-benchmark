package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.gui.Styles.{Editors => *}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
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
        case Duration(t, u) => Some(Some(FiniteDuration(t, u)))
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

    <.input.text(
      *.inputText(validity),
      ^.disabled := p.enabled.is(Disabled),
      ^.onChange ==> onChange,
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
