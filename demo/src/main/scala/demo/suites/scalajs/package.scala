package demo.suites

import demo.Libraries
import japgolly.scalajs.benchmark.gui.GuiBuilder

package object scalajs {

  lazy val all = GuiBuilder.folder(Libraries.ScalaJs.fullName)(
    Allocation.guiSuite,
  )
}
