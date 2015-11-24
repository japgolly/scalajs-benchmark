package demo.suites

import japgolly.scalajs.benchmark.gui.MenuComp

package object shootouts {

  lazy val all = MenuComp.folder("Shootouts")(
    FreeMonadShootout.guiSuite,
    LensShooutout.guiSuite)
}
