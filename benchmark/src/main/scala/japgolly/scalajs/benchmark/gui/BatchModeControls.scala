package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import Styles.{BatchMode => *}

object BatchModeControls {

  final case class Props(bms  : Int,
                         etaMs: Double,
                         start: Option[Option[Reusable[Callback]]],
                         abort: Option[Reusable[Callback]],
                         reset: Option[Reusable[Callback]],
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] = {
    implicit val d = Reusability.double(500)
    Reusability.byRef || Reusability.derive
  }

  private val button =
    <.button(*.controlButton)

  private def render(p: Props): VdomNode = {
    def kv(key: String, value: VdomNode) =
      <.tr(
        <.td(*.controlKey, key + ":"),
        <.td(*.controlValue, value))

    val startButton =
      p.start.whenDefined { oc =>
        button(
          ^.disabled := oc.isEmpty,
          ^.onClick --> Callback.traverseOption(oc)(_.value),
          "Start")
      }

    val abortButton =
      p.abort.whenDefined { cb =>
        button(^.onClick --> cb, "Abort")
      }

    val resetButton =
      p.reset.whenDefined { cb =>
        button(^.onClick --> cb, "Reset")
      }

    <.section(
      *.controlsSection,
      <.h3("Controls"),
      <.table(*.controlTable,
        <.tbody(
          kv("Benchmarks", p.bms),
          kv("ETA", GuiUtil.formatETA(p.etaMs)),
        )
      ),
      <.div(*.controlButtonRow,
        startButton,
        abortButton,
        resetButton,
      )
    )
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}