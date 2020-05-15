package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.gui.GuiSuite
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import Styles.{Menu => *}

object TableOfContents {
  import Router.Page

  sealed trait Item

  object Item {
    sealed trait WithPage extends Item {
      val urlPath: String
    }

    final case class Folder   (name: String, children: Seq[Item])   extends Item
    final case class Suite    (urlPath: String, suite: GuiSuite[_]) extends WithPage
    final case class BatchMode(urlPath: String)                     extends WithPage
  }

  final case class Props(items      : Seq[Item],
                         router     : Router.Ctl,
                         headerStyle: TagMod = *.folder,
                         ulStyle    : TagMod = *.folderUL,
                         liStyle    : TagMod = *.folderLI,
                        ) {
    @inline def render: VdomElement = Component(this)
  }

  private def render(p: Props) = {
    val li = <.li(p.liStyle)

    def children(items: Seq[Item]): VdomTag =
      <.ul(
        p.ulStyle,
        items.iterator.zipWithIndex.toVdomArray(x =>
          li(^.key := x._2, go(x._1))))

    def go(item: Item): VdomTag =
      item match {
        case i: Item.Folder    => <.div(<.h3(p.headerStyle, i.name), children(i.children))
        case i: Item.Suite     => p.router.link(Page.Suite(i))(i.suite.name)
        case i: Item.BatchMode => p.router.link(Page.BatchMode(i))(BatchMode.name)
      }

    children(p.items)
  }

  val Component =
    ScalaComponent.builder[Props]
      .render_P(render)
      .build
}