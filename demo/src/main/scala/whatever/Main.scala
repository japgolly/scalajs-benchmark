package whatever

import japgolly.scalajs.benchmark.gui.BenchmarkGUI
import org.scalajs.dom.document
import scalajs.js

object Main extends js.JSApp {

  def main(): Unit = {

    val tgt = document.getElementById("body")

    BenchmarkGUI.renderMenu(tgt)(Demos.all)

//    BenchmarkGUI.renderSuite(tgt)(Demos.noParams)
  }
}
