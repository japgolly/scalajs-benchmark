package whatever

import concurrent.duration._
import japgolly.scalajs.benchmark.engine.Options
import japgolly.scalajs.benchmark.gui.BenchmarkGUI
import org.scalajs.dom.document
import scalajs.js

object Main extends js.JSApp {

  def main(): Unit = {

    val tgt = document.getElementById("body")

//    val opts = Options.Default.copy(minRuns = 1000, minTime = 0.millis)
//    BenchmarkGUI.renderMenu(tgt, options = opts)(Demos.all)

    BenchmarkGUI.renderMenu(tgt)(Demos.all)

    //    BenchmarkGUI.renderSuite(tgt)(Demos.noParams)
  }
}
