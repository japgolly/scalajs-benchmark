package whatever

import scalacss.Defaults._
import scalacss.ScalaCssReact._

object Styles extends StyleSheet.Inline {
  import dsl._

  object ResultTable {

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
      fontFamily := "monospace"
    )
  }

  // ===================================================================================================================
  initInnerObjects(
    ResultTable.table)
}
