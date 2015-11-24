package demo.suites

import japgolly.scalajs.benchmark.gui.MenuComp

package object cats {

  lazy val all = MenuComp.folder("Cats")(
    FreeMonads.guiSuite,
    TrampolineBM.guiSuite)
}
