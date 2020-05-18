package demo.suites

import japgolly.scalajs.benchmark.gui.GuiBuilder

package object shootouts {

  lazy val all = GuiBuilder.folder("Shootouts")(
    FreeMonadShootout.guiSuite,
    StateMonadShootout.guiSuite,
    LensShooutout.guiSuite)
}
