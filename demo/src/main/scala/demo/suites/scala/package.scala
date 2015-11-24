package demo.suites

import japgolly.scalajs.benchmark.gui.MenuComp

package object scala {

  lazy val all = MenuComp.folder("Scala")(
    BuildSet.guiSuite,
    Flatmap.guiSuite,
    IntSet.guiSuite,
    VectorIndex.guiSuite)
}
