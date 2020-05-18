package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react.vdom.html_<^._

final case class LayoutConfig(toc      : VdomElement => VdomElement,
                              suite    : LayoutConfig.ArgsWithNav => VdomElement,
                              batchMode: LayoutConfig.ArgsWithNav => VdomElement)
object LayoutConfig {

  final case class ArgsWithNav(nav: VdomElement, page: VdomElement) {
    def default = <.div(nav, page)
  }

  def default =
    LayoutConfig(
      toc       = identity,
      suite     = _.default,
      batchMode = _.default,
    )
}
