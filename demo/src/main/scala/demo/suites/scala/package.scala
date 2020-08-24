package demo.suites

import demo.Libraries
import japgolly.scalajs.benchmark.gui.GuiBuilder

package object scala {

  lazy val all = GuiBuilder.folder(Libraries.Scala.fullName)(
    Builders.guiSuite,
    MapBuilding.guiSuite,
    SetBuilding.guiSuite,
    Flatmap.guiSuite,
    IntSet.guiSuite,
    VectorIndex.guiSuite,
  )
}
