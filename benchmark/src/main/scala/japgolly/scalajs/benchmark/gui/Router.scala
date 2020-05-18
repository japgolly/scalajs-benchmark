package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.benchmark.gui.Styles.{Menu => *}
import japgolly.scalajs.benchmark.gui.TableOfContents.Item
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import scalacss.ScalaCssReact._

object Router {

  type Ctl = RouterCtl[Page]

  sealed trait Page
  object Page {
    case object Index extends Page
    final case class Suite(value: Item.Suite) extends Page
    final case class BatchMode(value: Item.BatchMode) extends Page
  }

  // ===================================================================================================================

  def config(items        : Seq[Item],
             layoutConfig : LayoutConfig,
             engineOptions: EngineOptions,
             guiOptions   : GuiOptions): RouterConfig[Page] = {

    val idx = index(items)

    RouterConfigDsl[Page].buildConfig { dsl =>
      import dsl._

      val rootRoute: Rule =
        staticRoute(root, Page.Index) ~> renderR(rc => TableOfContents.Props(items, rc).render)

      val routes =
        idx.foldLeft(rootRoute) {

          case (q, (path, i: Item.Suite)) =>
            val vdom = SuiteRunner.render(i.suite, engineOptions, guiOptions)
            q | staticRoute(path, Page.Suite(i)) ~> render(vdom)

          case (q, (path, i: Item.BatchMode)) =>
            val is = items.collect { case x: Item.NonBatchMode => x }
            val props = BatchMode.Props(is, engineOptions, guiOptions)
            q | staticRoute(path, Page.BatchMode(i)) ~> render(props.render)
        }

      (routes | trimSlashes)
        .notFound(redirectToPage(Page.Index)(SetRouteVia.HistoryReplace))
        .renderWith(renderLayout(layoutConfig))
    }
  }

  private def index(mis: Seq[Item]): Map[String, Item.WithPage] = {
    var m = Map.empty[String, Item.WithPage]
    def add(i: Item.WithPage, path: String): Unit = {
      if (m contains path)
        dom.console.error(s"Multiple suites detected at URL: $path")
      m = m.updated(path, i)
    }
    def go(mis: Seq[Item]): Unit =
      mis.foreach {
        case i: Item.Folder   => go(i.children)
        case i: Item.WithPage => add(i, i.urlPath)
      }
    go(mis)
    m
  }

  // ===================================================================================================================

  private def renderLayout(cfg: LayoutConfig)(ctl: Ctl, res: Resolution[Page]): VdomElement = {
    val crumbSep = <.span(*.topNavBreadcrumbSep, "/")

    def breadcrumb(path: String): TagMod =
      path
        .dropWhile { case '#' | '/' => true; case _ => false }.split('/')
        .iterator.zipWithIndex.toTagMod { case (frag, i) =>
        val x = <.span(frag)
        if (i == 0) x else TagMod(crumbSep, frag)
      }

    def renderNav(name: TagMod): VdomElement =
      <.div(
        *.topNav,
        ctl.link(Page.Index)("Home"),
        crumbSep,
        name)

    res.page match {
      case Page.Index =>
        cfg.toc(res.render())

      case Page.Suite(i) =>
        val args = LayoutConfig.ArgsWithNav(
          nav = renderNav(breadcrumb(i.urlPath)),
          page = res.render()
        )
        cfg.suite(args)

      case Page.BatchMode(_) =>
        val args = LayoutConfig.ArgsWithNav(
          nav = renderNav(BatchMode.name),
          page = res.render()
        )
        cfg.batchMode(args)
    }
  }

}
