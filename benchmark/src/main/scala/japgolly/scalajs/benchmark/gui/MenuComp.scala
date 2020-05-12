package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.react._, vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.{RouterCtl => RouterCtl_, _}
import org.scalajs.dom
import scalacss.ScalaCssReact._
import Styles.{Menu => *}

/**
  * A React component that provides a menu of benchmarking suites, and allows the user to navigate around them.
  */
object MenuComp {

  final case class LayoutCfg(topPage: VdomElement => VdomElement,
                             suitePage: (TagMod => VdomElement, VdomElement) => VdomElement)
  object LayoutCfg {
    def default =
      LayoutCfg(identity, (nav, page) => <.div(nav(EmptyVdom), page))
  }

  final case class UrlFrag(path: String)
  object UrlFrag {
    def from(string: String): UrlFrag =
      UrlFrag(string.toLowerCase.replaceAll("[^a-zA-Z0-9-]+", "_"))
  }

  sealed trait MenuItem
  final case class MenuSuite(urlFrag: UrlFrag, suite: GuiSuite[_]) extends MenuItem
  final case class MenuFolder(name: String, urlFrag: UrlFrag, children: MenuItems) extends MenuItem
  type MenuItems = Iterable[MenuItem]

  implicit def autoLiftGuiSuite(s: GuiSuite[_]): MenuSuite =
    MenuSuite(UrlFrag from s.name, s)

  implicit def autoSoleMenuItem[T](t: T)(implicit f: T => MenuItem): MenuItems =
    f(t) :: Nil

  def folder(name: String, urlFrag: UrlFrag = null)(c: MenuItem*): MenuFolder = {
    val cs = c.iterator
      .map[((Int, String), MenuItem)] {
        case m: MenuSuite => ((1, m.suite.name), m)
        case m: MenuFolder => ((0, m.name), m)
      }
      .toVector
      .sortBy(_._1)
      .map(_._2)
    folderUnsorted(name, urlFrag)(cs: _*)
  }

  def folderUnsorted(name: String, urlFrag: UrlFrag = null)(c: MenuItem*): MenuFolder = {
    val uf = Option(urlFrag).getOrElse(UrlFrag from name)
    MenuFolder(name, uf, c)
  }

  def buildRouter(baseUrl: BaseUrl,
                  layout: LayoutCfg = LayoutCfg.default,
                  options: EngineOptions = EngineOptions.default)
                 (m1: MenuItems, mn: MenuItems*): Router[_] = {
    val mis = m1.toVector ++ mn.flatten
    val mis2 = Internals.convert(mis)
    val cfg = Internals.routerCfg(mis2, layout, options)
    Router(baseUrl, cfg)
  }

  // ↑ Public stuff
  // ===================================================================================================================
  // ↓ Internals

  private object Internals {

    sealed trait MenuItem2
    type MenuItems2 = Iterable[MenuItem2]
    case class MenuSuite2(urlPath: String, suite: GuiSuite[_]) extends MenuItem2
    case class MenuFolder2(name: String, children: MenuItems2) extends MenuItem2

    type RouterCtl = RouterCtl_[Page]

    type Page = Option[MenuSuite2]

    def convert(mis: MenuItems): MenuItems2 = {
      def go(path: String, mi: MenuItem): MenuItem2 = {
        def addFrag(suffix: UrlFrag): String =
          path + suffix.path

        mi match {
          case i: MenuSuite =>
            MenuSuite2(addFrag(i.urlFrag), i.suite)
          case i: MenuFolder =>
            val prefix = addFrag(i.urlFrag) + "/"
            MenuFolder2(i.name, i.children.map(go(prefix, _)))
        }
      }
      mis map (go("#/", _))
    }

    def index(mis: MenuItems2): Map[String, MenuSuite2] = {
      var m = Map.empty[String, MenuSuite2]
      def go(mis: MenuItems2): Unit =
        mis foreach {
          case i: MenuFolder2 => go(i.children)
          case i: MenuSuite2 =>
            val path = i.urlPath
            if (m contains path)
              dom.console.error(s"Multiple suites detected at URL: $path")
            m = m.updated(path, i)
        }
      go(mis)
      m
    }

    def routerCfg(items: MenuItems2, layoutCfg: LayoutCfg, options: EngineOptions): RouterConfig[Page] = {
      val idx = index(items)

      RouterConfigDsl[Page].buildConfig { dsl =>
        import dsl._

        val rootRoute: Rule =
          staticRoute(root, None) ~> renderR(rc => TOC.Comp(TOC.Props(items, rc)))

        val routes =
          idx.foldLeft(rootRoute){ case (q, (path, mi)) =>
            q | staticRoute(path, Some(mi)) ~> render(SuiteComp.Comp(SuiteComp.Props(mi.suite, options)))
          }

        (routes | trimSlashes)
          .notFound(redirectToPage(None)(SetRouteVia.HistoryReplace))
          .renderWith(layout(layoutCfg))
          .verify(None, idx.valuesIterator.map(Some(_)).toList: _*)
      }
    }

    val crumbSep = <.span(*.topNavBreadcrumbSep, "/")

    def layout(layoutCfg: LayoutCfg)(ctl: RouterCtl, res: Resolution[Page]): VdomElement =
      res.page match {
        case None =>
          layoutCfg topPage res.render()

        case Some(mi) =>
          def breadcrumb = mi.urlPath
            .dropWhile { case '#' | '/' => true; case _ => false }.split('/')
            .iterator.zipWithIndex.toTagMod { case (frag, i) =>
              val x = <.span(frag)
              if (i == 0) x else TagMod(crumbSep, frag)
            }

          def topNav(tm: TagMod): VdomElement =
            <.div(
              *.topNav,
              ctl.link(None)("Home"),
              crumbSep,
              breadcrumb)

          layoutCfg.suitePage(topNav, res.render())
      }

    object TOC {
      final case class Props(items      : MenuItems2,
                             router     : RouterCtl,
                             headerStyle: TagMod = *.folder,
                             ulStyle    : TagMod = *.folderUL,
                             liStyle    : TagMod = *.folderLI,
                            )

      private def render(p: Props) = {
        val li = <.li(p.liStyle)

        def children(items: MenuItems2): VdomTag =
          <.ul(
            p.ulStyle,
            items.iterator.zipWithIndex.toVdomArray(x =>
              li(^.key := x._2, go(x._1))))

        def go(mi: MenuItem2): VdomTag =
          mi match {
            case s: MenuSuite2  => p.router.link(Some(s))(s.suite.name)
            case s: MenuFolder2 => <.div(<.h3(p.headerStyle, s.name), children(s.children))
          }

        children(p.items)
      }

      val Comp =
        ScalaComponent.builder[Props]
          .render_P(render)
          .build
    }
  }
}
