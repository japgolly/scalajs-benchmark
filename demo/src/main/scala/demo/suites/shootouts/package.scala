package demo.suites

import japgolly.scalajs.benchmark.gui.GuiBuilder

package object shootouts {

  lazy val all = GuiBuilder.folder("Shootouts")(
    AsyncEffectShootout.guiSuite,
    FreeMonadShootout.guiSuite,
    LensShooutout.guiSuite,
    StateMonadShootout.guiSuite,
  )
}
