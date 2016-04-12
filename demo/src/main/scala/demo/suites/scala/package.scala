package demo.suites

import japgolly.scalajs.benchmark.gui.MenuComp

package object scala {

  lazy val all = MenuComp.folder("Scala")(
    MapBuilding.guiSuite,
    SetBuilding.guiSuite,
    Flatmap.guiSuite,
    IntSet.guiSuite,
    VectorIndex.guiSuite)
}
