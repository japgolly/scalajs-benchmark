package japgolly.scalajs.benchmark

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.ExternalVar
import japgolly.scalajs.react.vdom.prefix_<^._

package object gui {

  case class GuiSuite[P](suite: Suite[P], params: Params[P])

}
