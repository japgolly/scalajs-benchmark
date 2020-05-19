package demo.suites

import demo.Libraries
import japgolly.scalajs.benchmark.gui.GuiBuilder

package object scalaz {

  lazy val all = GuiBuilder.folder(Libraries.Scalaz.fullName)(
    FreeMonads.guiSuite)
}
