package demo.suites

import japgolly.scalajs.benchmark.gui.MenuComp

package object scalaz {

  lazy val all = MenuComp.folder("Scalaz")(
    FreeMonads.guiSuite)
}
