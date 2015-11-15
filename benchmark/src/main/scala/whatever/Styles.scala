package whatever

import scalacss.Defaults._
import scalacss.ScalaCssReact._

object Styles extends StyleSheet.Inline {
  import dsl._

  object ResultTable {
    import whatever.ReactChart.ScalaDataset

    val table = style(
      borderCollapse.collapse,
      unsafeChild("td,th")(
        border(solid, 1.px, black),
        padding(v = 0.2 ex, h = 0.6 ex)
      ))

    val resultHeader = style(
      minWidth(12 ex),
      textAlign.center)

    val numericResult = style(
      textAlign.right,
      fontFamily := "monospace")

    def paramInt = numericResult

    val paramBool = style(
      textAlign.center)

    val graph = style(
      marginTop(2 em),
      width(400 px),
      height(400 px))

    def styleDataset(d: ScalaDataset): ScalaDataset =
      d.copy(
        fillColor       = "#FFC870",
        strokeColor     = "#FFC870",
        highlightFill   = "#FDB45C",
        highlightStroke = "#FDB45C")
  }

  // ===================================================================================================================
  initInnerObjects(
    ResultTable.table)
}
