package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import japgolly.scalajs.benchmark.gui.Styles.{TextOutput => *}
import japgolly.scalajs.benchmark.vendor.FileSaver
import org.scalajs.dom.raw.{Blob, BlobPropertyBag}
import org.scalajs.dom.{document, html}
import scala.scalajs.js

object TextOutput {

  final case class Props(text: String, mimeType: String, filename: String) {
    @inline def render: VdomElement = Component(this)
  }

  object Props {
    implicit def reusability: Reusability[Props] =
      Reusability.derive
  }

  final case class State(prev: Props, clicked: Boolean) {
    def update(props: Props): State =
      if (props == prev)
        this
      else
        State.init(props)
  }

  object State {
    def init(props: Props): State =
      apply(props, clicked = false)

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

    private val button =
      <.button(^.role := "button")

    def render(p: Props, s: State): VdomNode = {

      val copyButtonLabel =
        if (s.clicked)
          "Copied"
        else
          "Copy to clipboard"

      val saveFile = Callback {
        val body = js.Array[js.Any](p.text)
        val mime = BlobPropertyBag(p.mimeType + ";charset=utf-8")
        val blob = new Blob(body, mime)
        FileSaver.saveAs(blob, p.filename)
      }

      def copyButton = TagMod(
        <.textarea.withRef(hiddenTextAreaRef)(
          *.hiddenTextArea,
          ^.tabIndex    := -1,
          ^.aria.hidden := true,
          ^.readOnly    := true,
          ^.value       := p.text),
        button(
          ^.onClick --> copyToClipboard,
          copyButtonLabel))

      val saveButton =
        button(
          ^.onClick --> saveFile,
          "Save")

      <.pre(*.pre,
        <.div(*.buttons,
          copyButton,
          saveButton,
        ),
        p.text,
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