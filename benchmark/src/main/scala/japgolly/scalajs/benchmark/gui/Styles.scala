package japgolly.scalajs.benchmark.gui

import CssSettings._
import scalacss.ScalaCssReact._

object Styles extends StyleSheet.Inline {
  import dsl._

//  val cssReset = style(scalacss.ext.CssReset.normaliseCss)

  object Suite {

    val suiteName = style(
      fontSize(2 em),
      margin.vertical(0.8 em))

    val suiteDesc = style(
      maxWidth(90 %%),
      boxSizing.borderBox,
      marginBottom(1.5 em),
      padding(0.8 ex),
      border(solid, 1.px, black),
      backgroundColor(c"#d7ebff"),
      unsafeChild("code")(
        backgroundColor(c"#eee"),
        padding(1 px, 3 px),
        margin.horizontal(2 px),
        border(solid, 1 px, c"#bbb")))

    private val anyHeader = mixin(
      backgroundColor(c"#eee"))

    private val settingsCell = mixin(
      border(solid, 1.px, black),
      padding.vertical(1 ex))

    val settingsTable = style(
      borderCollapse.collapse,
      marginBottom(1 em))

    val settingsTableHeader = style(
      settingsCell,
      textAlign.right,
      padding.horizontal(2 ex),
      anyHeader)

    val settingsTableData = style(
      settingsCell,
      padding.horizontal(1 ex),
      textAlign.left)

    val settingsTableBm = style(
      display.block)

    val settingsTableBmLabel = style(
      fontWeight.normal,
      paddingLeft(0.3 ex))

    val resultTable = style(
      borderCollapse.collapse)

    private val resultCell = mixin(
      border(solid, 1.px, black))

    val resultHeader = style(
      anyHeader,
      resultCell,
      textAlign.center,
      padding(v = 0.5 ex, h = 1 ex))

    val resultHeaderScore = style(
      resultHeader,
      minWidth(12 ex))

    val resultData = style(
      resultCell,
      padding(v = 0.2 ex, h = 1 ex))

    val preparing = style(
      color(c"#33c"))

    val running = style(
      color(c"#d40000"))

    val numericResult = style(
      textAlign.right,
      fontFamily :=! "monospace")

    def paramInt = numericResult

    val paramBool = style(
      textAlign.center)

    val paramEnumLabel = style(
      settingsTableBmLabel,
      display.block)

    val startButton = style(
      &.disabled(color(c"#aaa")))

    val runningRow = style(
      marginBottom(1 ex))

    val abortButton = style(
      marginLeft(2 ex))

    val doneRow = style(
      color(green),
      runningRow)

    def resetButton = abortButton

    val resultFormatRow = style(
      marginTop(.25 em),
      marginBottom(.75 em))

    val resultFormat = style(
      cursor.pointer,
      marginLeft(1.5 ex))

    val resultText = style(
      border(solid, 1 px, c"#111"),
      backgroundColor(c"#e9e9e9"),
      padding(1 ex),
    )

    private def graphWidth = 640 px

    val graphContainer = style(
      marginTop(3 em),
      // padding(1 em),
      // border(solid, 1 px, c"#d4d4d4"),
      width(graphWidth))

    val graphHeader = style(
      width(graphWidth),
      textAlign.center,
      color(c"#555"),
      fontFamily :=! "sans-serif",
      fontSize(0.9 em))

    val graph = style(
      width(graphWidth),
      height(720 px))

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

  object Menu {

    val topNav = style(
      width(100 %%),
      boxSizing.borderBox,
      padding(v = 1 ex, h = 2 ex),
      backgroundColor(c"#eee"))

    val topNavBreadcrumbSep = style(
      color(c"#888"),
      margin.horizontal(1 ex))

    val folder = style(
      marginTop(0.6 em),
      marginBottom(0.3 em))

    val folderUL = style(
      marginBottom(1.2 em))

    val folderLI = style(
      marginBottom(0.3 em))
  }

  // ===================================================================================================================

  initInnerObjects(
    Menu.topNav,
    Suite.resultTable)
}
