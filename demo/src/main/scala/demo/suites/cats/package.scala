package demo.suites

import demo.Libraries
import japgolly.scalajs.benchmark.gui.GuiBuilder

package object cats {

  lazy val all = GuiBuilder.folder(Libraries.Cats.fullName)(
    FreeMonads.guiSuite,
    TrampolineBM.guiSuite)
}
