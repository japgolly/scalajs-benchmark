package demo.suites

import demo.scala.IntSet
import japgolly.scalajs.benchmark.gui.MenuComp

package object scala {

  lazy val all = MenuComp.folder("Scala")(
    IntSet.guiSuite)
}
