package japgolly.scalajs.benchmark

package object gui {

  class GuiSuite[P](val suite: Suite[P], val params: Params[P])

  object GuiSuite {

    def apply(suite: Suite[Unit]): GuiSuite[Unit] =
      new GuiSuite(suite, Params.none)

    def apply[P](suite: Suite[P], params: Params[P]): GuiSuite[P] =
      new GuiSuite(suite, params)
  }

}
