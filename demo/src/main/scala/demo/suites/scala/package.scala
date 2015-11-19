package demo.suites

import japgolly.scalajs.benchmark.gui.MenuComp

package object scala {

  lazy val all = MenuComp.folder("Scala")(
    IntSet.guiSuite,
    VectorIndex.guiSuite)
}
