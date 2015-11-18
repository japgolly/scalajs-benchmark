package whatever

import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import japgolly.scalajs.react.ReactDOM
import org.scalajs.dom
import scalajs.js
import scalacss.Defaults._
import scalacss.ScalaCssReact._

object Main extends js.JSApp {

  def main(): Unit = {
    import japgolly.scalajs.benchmark._
    import japgolly.scalajs.benchmark.gui.{Styles, SuiteComp}
    import whatever.newshit.Demos

    val s = Demos.oneParam

    Chart.defaults.global.animationSteps = 20

    Styles.addToDocument()
    val tgt = dom.document.getElementById("body")
    ReactDOM.render(SuiteComp.Comp(SuiteComp.Props(s)), tgt)
  }

    // router
    // library of bm suites
    // allow checkboxes beside BMs before starting to turn some off

    // would be good to keep everything on one screen (with expand/collapse) and allow run all/some
}
