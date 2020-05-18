package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.gui.Styles.{BatchMode => *}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.annotation.nowarn
import scalacss.ScalaCssReact._

object BatchModeControls {

  final case class Props(completedBMs : Int,
                         bms          : Int,
                         elapsedMs    : Double,
                         etaMs        : Double,
                         formats      : Map[SuiteResultsFormat.Text, Enabled],
                         updateFormats: Option[Map[SuiteResultsFormat.Text, Enabled] ~=> Callback],
                         start        : Option[Option[Reusable[Callback]]],
                         abort        : Option[Reusable[Callback]],
                         reset        : Option[Reusable[Callback]],
                         downloadTest : Boolean,
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] = {
    @nowarn("cat=unused") implicit val d = Reusability.double(499) // 499ms tolerance because we're rendering ETA with second-precision
    @nowarn("cat=unused") implicit val f: Reusability[Map[SuiteResultsFormat.Text, Enabled]] = Reusability.byRef
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
        <.div(p.formats.toArray.sortBy(_._1.label).toTagMod { case (fmt, enabled) =>
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

    // Chrome, for example, will save one file then display a popup asking if
    // its ok to save the rest. This button allows users to give permission
    // before starting a BM (so that they can walk away after hitting start).
    val downloadPrepButton =
      TagMod.when(p.downloadTest) {
        val save = Callback.byName {
          Callback.traverse(List(1, 2))(i =>
            GuiUtil.saveFile(text = "", filename = s"test-$i.tmp", "text/plain"))
        }
        <.div(*.controlButtonRow,
          <.button(
            *.controlDownloadPrepButton,
            ^.onClick --> save,
            "Test download of", <.br, "multiple files"))
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
      ),
      downloadPrepButton,
    )
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}