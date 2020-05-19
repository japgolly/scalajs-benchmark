package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.benchmark.gui.CssSettings._
import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import japgolly.scalajs.react.extra.router.BaseUrl
import japgolly.scalajs.react.raw.ReactDOM.Container
import scalacss.ScalaCssReact._

object BenchmarkGUI {

  private var init = true
  def initialise(): Unit =
    if (init) {
      Chart.defaults.global.animationSteps = 20
      Styles.addToDocument()
      init = false
    }

  def defaultBaseUrl(): BaseUrl =
    BaseUrl.until_#

  def renderMenu(container    : Container,
                 baseUrl      : BaseUrl       = defaultBaseUrl(),
                 layoutConfig : LayoutConfig  = LayoutConfig.default,
                 engineOptions: EngineOptions = EngineOptions.default,
                 guiOptions   : GuiOptions    = GuiOptions.default,
                )(m1: Seq[GuiBuilder.MenuItem], mn: Seq[GuiBuilder.MenuItem]*): Unit = {
    initialise()
    val router = GuiBuilder.router(baseUrl, layoutConfig, engineOptions, guiOptions)(m1, mn: _*)
    router().renderIntoDOM(container)
  }

  def renderSuite[P](container    : Container,
                     engineOptions: EngineOptions = EngineOptions.default,
                     guiOptions   : GuiOptions    = GuiOptions.default,
                    )
                    (s: GuiSuite[P]): Unit = {
    initialise()
    val vdom = SuiteRunner.render(s, engineOptions, guiOptions)
    vdom.renderIntoDOM(container)
  }
}
