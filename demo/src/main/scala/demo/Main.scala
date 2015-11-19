package demo

import org.scalajs.dom.document
import scalajs.js
import japgolly.scalajs.benchmark.gui.BenchmarkGUI

object Main extends js.JSApp {

  def main(): Unit = {

    // import concurrent.duration._
    // import japgolly.scalajs.benchmark.engine.Options
    // val opts = Options.Default.copy(minRuns = 1000, minTime = 0.millis)

    val tgt = document.getElementById("body")

    BenchmarkGUI.renderMenu(tgt)(
      suites.example.Examples.all,
      suites.scala.all)

    // BenchmarkGUI.renderSuite(tgt)(suites.example.Examples.oneParam)
  }
}