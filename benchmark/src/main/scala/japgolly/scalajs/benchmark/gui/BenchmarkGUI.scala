package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.Options
import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.extra.router.BaseUrl
import org.scalajs.dom
import scalacss.Defaults._
import scalacss.ScalaCssReact._
import MenuComp.MenuItems

object BenchmarkGUI {

  private var init = true
  def initialise(): Unit =
    if (init) {
      Chart.defaults.global.animationSteps = 20
      Styles.addToDocument()
      init = false
    }

  def defaultBaseUrl(): BaseUrl =
    BaseUrl(dom.window.location.href.takeWhile(_ != '#'))

  def renderMenu(container: dom.Node, baseUrl: BaseUrl = defaultBaseUrl(), options: Options = Options.Default)
                (m1: MenuItems, mn: MenuItems*): Unit = {
    initialise()
    val router = MenuComp.buildRouter(baseUrl, options)(m1, mn: _*)
    ReactDOM.render(router(), container)
  }

  def renderSuite[P](container: dom.Node, options: Options = Options.Default)(s: GuiSuite[P]): Unit = {
    initialise()
    val p = SuiteComp.Props(s, options)
    ReactDOM.render(SuiteComp.Comp(p), container)
  }
}
