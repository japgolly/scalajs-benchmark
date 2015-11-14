package whatever

import scalajs.js

object Main extends js.JSApp {
  def main(): Unit = {
    println("start")

    val s = IntSet_X.suite
    Benchy.runSuite(s)

    println("end")
  }
}
