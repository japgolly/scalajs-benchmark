package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import Styles.{BatchMode => *}

object BatchModeControls {

  final case class Props(completedBMs : Int,
                         bms          : Int,
                         elapsedMs    : Double,
                         etaMs        : Double,
                         formats      : Map[FormatResults.Text, Enabled],
                         updateFormats: Option[Map[FormatResults.Text, Enabled] ~=> Callback],
                         start        : Option[Option[Reusable[Callback]]],
                         abort        : Option[Reusable[Callback]],
                         reset        : Option[Reusable[Callback]],
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] = {
    implicit val d = Reusability.double(500)
    implicit val f: Reusability[Map[FormatResults.Text, Enabled]] = Reusability.byRef
    Reusability.byRef || Reusability.derive
  }

  private val button =
    <.button(*.controlButton)

  private def render(p: Props): VdomNode = {
    def kv(key: String, value: VdomNode) =
      <.tr(
        <.td(*.controlKey, key + ":"),
        <.td(*.controlValue, value))

    val completed =
      TagMod.when(p.start.isEmpty) {
        val pct = p.completedBMs * 100 / p.bms
        kv("Completed", s"${p.completedBMs} (${pct}%)")
      }

    val elapsed =
      TagMod.when(p.start.isEmpty) {
        kv("Elapsed", GuiUtil.formatETA(p.elapsedMs))
      }

    val eta =
      TagMod.when(p.reset.isEmpty) {
        kv("ETA", GuiUtil.formatETA(p.etaMs))
      }

    val formats =
      kv("Save results as", {
        val array = p.formats.toArray
        array.sortInPlaceBy(_._1.label)
        <.div(array.toTagMod { case (fmt, enabled) =>
          <.div(
            <.label(
              *.controlFormat,
              <.input.checkbox(
                ^.checked := enabled.is(Enabled),
                ^.disabled := p.updateFormats.isEmpty,
                ^.onChange -->? p.updateFormats.map(_ (p.formats.updated(fmt, !enabled))),
              ),
              fmt.label))
        })
      })

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
          formats,
          kv("Benchmarks", p.bms),
          completed,
          elapsed,
          eta,
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