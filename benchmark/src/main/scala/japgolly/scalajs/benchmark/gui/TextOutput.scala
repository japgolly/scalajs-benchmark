package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import japgolly.scalajs.benchmark.gui.Styles.{TextOutput => *}
import org.scalajs.dom.{html, document}

object TextOutput {

  type Props = String

  final case class State(prevText: String, clicked: Boolean) {
    def update(text: String): State =
      if (text == prevText)
        this
      else
        State.init(text)
  }

  object State {
    def init(text: String): State =
      apply(text, clicked = false)

    implicit def reusability: Reusability[State] =
      Reusability.derive
  }

  final class Backend($: BackendScope[Props, State]) {

    private val hiddenTextAreaRef = Ref[html.TextArea]

    private val copyToClipboard: Callback =
      for {
        textArea <- hiddenTextAreaRef.get
        _        <- Callback{ textArea.select(); document.execCommand("copy") }
        _        <- $.modState(_.copy(clicked = true))
      } yield ()

    def render(text: Props, state: State): VdomNode = {

      val copyButtonLabel =
        if (state.clicked)
          "Copied"
        else
          "Copy to clipboard"

      def copyButton = TagMod(
        <.textarea.withRef(hiddenTextAreaRef)(
          *.hiddenTextArea,
          ^.tabIndex    := -1,
          ^.aria.hidden := true,
          ^.readOnly    := true,
          ^.value       := text),
        <.button(
          *.copyToClipboardButton,
          ^.role := "button",
          ^.onClick --> copyToClipboard,
          copyButtonLabel))

      <.pre(
        *.pre,
        copyButton,
        text,
      )
    }
  }

  val Component = ScalaComponent.builder[Props]("TextOutput")
    .initialStateFromProps(State.init)
    .renderBackend[Backend]
    .getDerivedStateFromProps((p, s) => s.update(p))
    .configure(Reusability.shouldComponentUpdate)
    .build
}