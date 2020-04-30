package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import japgolly.scalajs.react.raw.ReactDOM.Container
import japgolly.scalajs.react.extra.router.BaseUrl
import org.scalajs.dom
import scalacss.ScalaCssReact._
import CssSettings._
import MenuComp.{LayoutCfg, MenuItems}

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

  def renderMenu(container: Container,
                 baseUrl: BaseUrl = defaultBaseUrl(),
                 layout: LayoutCfg = LayoutCfg.default,
                 options: EngineOptions = EngineOptions.default)
                (m1: MenuItems, mn: MenuItems*): Unit = {
    initialise()
    val router = MenuComp.buildRouter(baseUrl, layout, options)(m1, mn: _*)
    router().renderIntoDOM(container)
  }

  def renderSuite[P](container: Container,
                     options: EngineOptions = EngineOptions.default)
                    (s: GuiSuite[P]): Unit = {
    initialise()
    val p = SuiteComp.Props(s, options)
    SuiteComp.Comp(p).renderIntoDOM(container)
  }
}
