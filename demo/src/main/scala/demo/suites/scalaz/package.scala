package demo.suites

import demo.Libraries
import japgolly.scalajs.benchmark.gui.MenuComp

package object scalaz {

  lazy val all = MenuComp.folder(Libraries.Scalaz.fullName)(
    FreeMonads.guiSuite)
}
