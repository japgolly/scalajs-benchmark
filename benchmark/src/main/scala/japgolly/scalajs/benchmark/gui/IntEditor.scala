package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.gui.Styles.{Editors => *}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scala.util.Try
import scalacss.ScalaCssReact._

object IntEditor {

  object Validator {

    def atLeast(n: Int): Int ~=> Option[Int] =
      Reusable.fn(i => if (i >= n) Some(i) else None)

    val atLeast0 = atLeast(0)
    val atLeast1 = atLeast(1)
  }

  final case class Props(state   : StateSnapshot[State],
                         validate: Int ~=> Option[Int],
                         style   : Reusable[TagMod],
                         enabled : Enabled) {

    val parsed: Option[Int] =
      state.value.parsed.flatMap(validate)

    val validity: Validity =
      Valid.when(parsed.isDefined)

    @inline def render: VdomElement = Component(this)
  }

  final case class State(text: String) {
    val parsed: Option[Int] =
      Try(text.trim.toInt).toOption
  }

  object State {
    def init(n: Int): State =
      apply(n.toString)
  }

  private val illegalChars = "[^0-9-]+".r

  private def render(p: Props): VdomNode = {
    val s = p.state.value

    def onChange(e: ReactEventFromInput): Callback =
      e.extract(_.target.value)(t => p.state.setState(State(illegalChars.replaceAllIn(t, ""))))

    <.input.text(
      *.inputText(p.validity),
      ^.disabled := p.enabled.is(Disabled),
      ^.onChange ==> onChange,
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