package whatever

import scalajs.js


object Main extends js.JSApp {
  def main(): Unit = {

    val s = IntSet_X.suite
//    Benchy.runSuite(s)

    import Benchy._
    runToConsole(s)

  }
}
