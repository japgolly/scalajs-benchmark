package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

// TODO Remove all these stupid Comp suffixes
object BatchComp {

  final case class Props() {
    @inline def render: VdomElement = Component(this)
  }

  //implicit val reusabilityProps: Reusability[Props] =
  //  Reusability.derive

  final class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props): VdomNode =
      <.div
  }

  val Component = ScalaComponent.builder[Props]("BatchComp")
    .renderBackend[Backend]
    //.configure(Reusability.shouldComponentUpdate)
    .build
}