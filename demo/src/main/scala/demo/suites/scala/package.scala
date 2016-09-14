package demo.suites

import demo.Libraries
import japgolly.scalajs.benchmark.gui.MenuComp

package object scala {

  lazy val all = MenuComp.folder(Libraries.Scala.fullName)(
    Builders.guiSuite,
    MapBuilding.guiSuite,
    SetBuilding.guiSuite,
    Flatmap.guiSuite,
    IntSet.guiSuite,
    VectorIndex.guiSuite)
}
