package whatever

import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import japgolly.scalajs.react.ReactDOM
import japgolly.scalajs.react.extra.router.BaseUrl
import org.scalajs.dom
import scalajs.js
import scalacss.Defaults._
import scalacss.ScalaCssReact._

object Main extends js.JSApp {

  def main(): Unit = {
    import japgolly.scalajs.benchmark.gui.{MenuComp, Styles}

    Chart.defaults.global.animationSteps = 20
    Styles.addToDocument()

    val baseUrl = BaseUrl(dom.window.location.href.takeWhile(_ != '#'))

    val router = MenuComp.buildRouter(baseUrl, Demos.all)

    val tgt = dom.document.getElementById("body")
    ReactDOM.render(router(), tgt)
  }
}
