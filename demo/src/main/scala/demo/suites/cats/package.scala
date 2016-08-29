package demo.suites

import demo.Libraries
import japgolly.scalajs.benchmark.gui.MenuComp

package object cats {

  lazy val all = MenuComp.folder(Libraries.Cats.fullName)(
    FreeMonads.guiSuite,
    TrampolineBM.guiSuite)
}
