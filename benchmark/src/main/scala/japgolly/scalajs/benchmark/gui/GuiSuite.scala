package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark._

/**
  * A suite of benchmarks with additional info required to slap a GUI on top and present it to the user.
  *
  * If you don't need a GUI, then a plain [[Suite]] is all you need.
  */
final class GuiSuite[P](val suite: Suite[P], val params: Params[P]) {
  @inline def name = suite.name
}

object GuiSuite {
  def apply(suite: Suite[Unit]): GuiSuite[Unit] =
    new GuiSuite(suite, Params.none)

  def apply[P](suite: Suite[P], params: Params[P]): GuiSuite[P] =
    new GuiSuite(suite, params)
}
