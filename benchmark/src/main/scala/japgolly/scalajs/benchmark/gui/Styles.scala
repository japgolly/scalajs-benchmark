package japgolly.scalajs.benchmark.gui

import CssSettings._
import japgolly.univeq.UnivEq
import scalacss.ScalaCssReact._

object Styles extends StyleSheet.Inline {
  import dsl._

  val enabled = Domain.boolean.map(Enabled.when)

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

    val etaRow = style(
      marginBottom(1 em),
    )

    val allBMsCheckbox = style(
      display.block,
      paddingBottom(0.3 em),
      marginBottom(0.3 em),
      borderBottom(1 px, solid, c"#ccc"),
    )

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
      color(c"#060"),
      runningRow)

    def resetButton = abortButton

    val resultFormatRow = style(
      marginTop(.25 em),
      marginBottom(.75 em))

    val resultFormat = style(
      cursor.pointer,
      marginLeft(1.5 ex))

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

  object TextOutput {

    val pre = style(
      border(solid, 1 px, c"#222"),
      color(c"#222"),
      backgroundColor(c"#ebfaff"),
      padding(1 ex),
      maxHeight :=! "calc(28em + 1ex)",
      overflow.auto,
      position.relative, // this block is the anchor to the buttons
    )

    val hiddenTextArea = style(
      position.absolute,
      zIndex(-1),
      opacity(0),
    )

    val buttons = style(
      position.absolute,
      top(`0`),
      right(`0`),
      display.flex,
      flexDirection.column,
      alignItems.flexEnd,
    )
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

  object BatchMode {

    sealed trait Status {
      import Status._

      final def merge(s: Status): Status =
        (this, s) match {
          case (Disabled, x) => x
          case (x, Disabled) => x

          case (Done, x@(Pending | Preparing | Running | Incomplete | Done)) => x
          case (x@(Pending | Preparing | Running | Incomplete), Done) => x

          case (Pending, x@(Pending | Preparing | Running)) => x
          case (x@(Preparing | Running), Pending) => x

          case (Pending, Incomplete) => Incomplete
          case (Incomplete, Pending) => Incomplete

          case (Incomplete, x@(Incomplete | Preparing | Running)) => x
          case (x@(Preparing | Running), Incomplete) => x

          // These cases shouldn't happen in practice
          case (Running, Running | Preparing) => Running
          case (Preparing, Running)           => Running
          case (Preparing, Preparing)         => Preparing
        }
    }

    object Status {
      case object Disabled   extends Status
      case object Pending    extends Status
      case object Incomplete extends Status
      case object Preparing  extends Status
      case object Running    extends Status
      case object Done       extends Status

      val domain = Domain.ofValues[Status](
        Disabled,
        Pending,
        Incomplete,
        Preparing,
        Running,
        Done,
      )

      implicit def univEq: UnivEq[Status] = UnivEq.derive
    }

    private val menuSharedUL = styleS(
      lineHeight(1.5 em),
      listStyleType := "none")

    val root = style(
      display.flex)

    val controlsSection = style(
      paddingRight(4.8 rem))

    val controlTable = style(
      borderCollapse.collapse,
      marginBottom(2 em))

    def controlKey   = Suite.settingsTableHeader
    def controlValue = Suite.settingsTableData

    val controlButtonRow = style(
      textAlign.center)

    val controlButton = style(
      fontSize(120 %%),
      padding(0.4 em, 2 ex))

    val menuRootUL = style(
      menuSharedUL,
      paddingInlineStart(`0`))

    val menuUL = style(
      menuSharedUL,
      paddingInlineStart(3.6 ex))

    val menuLI = styleF(enabled)(e => styleS(
      mixinIf(e is Disabled)(color(c"#ccc"))))

    val runningItem = styleF(Status.domain)(s => styleS(
      display.inlineBlock,
      s match {
        case Status.Disabled   => styleS()
        case Status.Pending    => styleS()
        case Status.Incomplete => styleS(color(c"#7db2e8"))
        case Status.Preparing  => styleS(color(c"#ef9d06"), &.before(content := "'➜ '"))
        case Status.Running    => styleS(color(c"#ef9d06"), &.before(content := "'➜ '"))
        case Status.Done       => styleS(color(c"#008800"), &.before(content := "'✓ '"))
      }
    ))
  }

  // ===================================================================================================================

  initInnerObjects(
    Suite.resultTable,
    BatchMode.menuUL,
    Menu.topNav,
    TextOutput.pre,
  )
}
