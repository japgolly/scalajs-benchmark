package japgolly.scalajs.benchmark.gui

import scalacss.Defaults._
import scalacss.ScalaCssReact._

object Styles extends StyleSheet.Inline {
  import dsl._

  object ResultTable {

    val table = style(
      borderCollapse.collapse
      ,unsafeChild("td,th")(
        border(solid, 1.px, black),
        padding(v = 0.3 ex, h = 1.2 ex)
      )
    )

    val resultHeader = style(
      minWidth(12 ex),
      textAlign.center)

    val numericResult = style(
      textAlign.right,
      fontFamily := "monospace")

    def paramInt = numericResult

    val paramBool = style(
      textAlign.center)

    val startButton = style(
      &.disabled(color(c"#aaa")))

    val graphOuter = style(
      marginTop(2 em),
      width(400 px),
      height(400 px))

    import ReactChart._
    def graphInner(d: ScalaBarData): ScalaBarData = {
      /*
      // Using this totally-not-a-quick-hack effect will colour graph bars according to their value, green ↔ red
      import ReactChart.RGB
      import ReactChart.ColourByValue
      import ReactChart.ColourByValue.scaleFn
      val graphFX = Some(ColourByValue(
        fill = Some(scaleFn(RGB(32,255,32), RGB(255,32,32))),
        stroke = Some(scaleFn(RGB(0,92,0), RGB(92,0,0)))))
       */

      def styleDataset(d: ScalaDataset): ScalaDataset =
        d.copy(
          fillColor       = "#FFC870",
          strokeColor     = "#FFC870",
          highlightFill   = "#FDB45C",
          highlightStroke = "#FDB45C")

      d.copy(datasets = d.datasets.map(styleDataset))
    }
  }

  // ===================================================================================================================
  initInnerObjects(
    ResultTable.table)
}
