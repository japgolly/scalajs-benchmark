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

  final case class LayoutCfg(topPage  : VdomElement => VdomElement,
                             suitePage: (TagMod => VdomElement, VdomElement) => VdomElement,
                             batch    : (TagMod => VdomElement, VdomElement) => VdomElement)
  object LayoutCfg {
    def default =
      LayoutCfg(
        identity,
        (nav, page) => <.div(nav(EmptyVdom), page),
        (nav, page) => <.div(nav(EmptyVdom), page),
      )
  }

  final case class UrlFrag(path: String)
  object UrlFrag {
    def from(string: String): UrlFrag =
      UrlFrag(string.toLowerCase.replaceAll("[^a-zA-Z0-9-]+", "_"))
  }

  sealed trait MenuItem
  final case class MenuSuite(urlFrag: UrlFrag, suite: GuiSuite[_]) extends MenuItem
  final case class MenuFolder(name: String, urlFrag: UrlFrag, children: MenuItems) extends MenuItem
  final case class MenuBatch(urlFrag: UrlFrag) extends MenuItem
  type MenuItems = Iterable[MenuItem]

  implicit def autoLiftGuiSuite(s: GuiSuite[_]): MenuSuite =
    MenuSuite(UrlFrag from s.name, s)

  implicit def autoSoleMenuItem[T](t: T)(implicit f: T => MenuItem): MenuItems =
    f(t) :: Nil

  def folder(name: String, urlFrag: UrlFrag = null)(c: MenuItem*): MenuFolder = {
    val cs = c.iterator
      .map[((Int, String), MenuItem)] {
        case m: MenuFolder => ((0, m.name), m)
        case m: MenuSuite  => ((1, m.suite.name), m)
        case m: MenuBatch  => ((2, ""), m)
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

  def buildRouter(baseUrl   : BaseUrl,
                  layout    : LayoutCfg     = LayoutCfg.default,
                  options   : EngineOptions = EngineOptions.default,
                  guiOptions: GuiOptions    = GuiOptions.default)
                 (m1: MenuItems, mn: MenuItems*): Router[_] = {
    var mis = m1.toVector ++ mn.flatten
    if (guiOptions.allowBatchMode)
      mis :+= MenuBatch(UrlFrag("batch-mode")) // TODO ensure unique
    val mis2 = Internals.convert(mis)
    val cfg = Internals.routerCfg(mis2, layout, options, guiOptions)
    Router(baseUrl, cfg)
  }

  // ↑ Public stuff
  // ===================================================================================================================
  // ↓ Internals

  private object Internals {

    sealed trait MenuItem2
    sealed trait MenuItem2Page extends MenuItem2 {
      val urlPath: String
    }
    type MenuItems2 = Iterable[MenuItem2]
    case class MenuSuite2(urlPath: String, suite: GuiSuite[_]) extends MenuItem2Page
    case class MenuFolder2(name: String, children: MenuItems2) extends MenuItem2
    case class MenuBatch2(urlPath: String) extends MenuItem2Page

    type RouterCtl = RouterCtl_[Page]

    sealed trait Page
    object Page {
      case object Index extends Page
      final case class Suite(value: MenuSuite2) extends Page
      final case class Batch(value: MenuBatch2) extends Page
    }

    def convert(mis: MenuItems): MenuItems2 = {
      def go(path: String, mi: MenuItem): MenuItem2 = {
        def addFrag(suffix: UrlFrag): String =
          path + suffix.path

        mi match {
          case i: MenuFolder =>
            val prefix = addFrag(i.urlFrag) + "/"
            MenuFolder2(i.name, i.children.map(go(prefix, _)))

          case i: MenuSuite =>
            MenuSuite2(addFrag(i.urlFrag), i.suite)

          case i: MenuBatch =>
            MenuBatch2(addFrag(i.urlFrag))
        }
      }
      mis map (go("#/", _))
    }

    def index(mis: MenuItems2): Map[String, MenuItem2Page] = {
      var m = Map.empty[String, MenuItem2Page]
      def add(i: MenuItem2Page, path: String): Unit = {
        if (m contains path)
          dom.console.error(s"Multiple suites detected at URL: $path")
        m = m.updated(path, i)
      }
      def go(mis: MenuItems2): Unit =
        mis foreach {
          case i: MenuFolder2   => go(i.children)
          case i: MenuItem2Page => add(i, i.urlPath)
        }
      go(mis)
      m
    }

    def routerCfg(items     : MenuItems2,
                  layoutCfg : LayoutCfg,
                  options   : EngineOptions,
                  guiOptions: GuiOptions): RouterConfig[Page] = {
      val idx = index(items)

      RouterConfigDsl[Page].buildConfig { dsl =>
        import dsl._

        val rootRoute: Rule =
          staticRoute(root, Page.Index) ~> renderR(rc => TOC.Comp(TOC.Props(items, rc)))

        val routes =
          idx.foldLeft(rootRoute) {
            case (q, (path, m: MenuSuite2)) =>
              q | staticRoute(path, Page.Suite(m)) ~> render(SuiteComp.Props(m.suite, options, guiOptions).render)
            case (q, (path, m: MenuBatch2)) =>
              q | staticRoute(path, Page.Batch(m)) ~> render(BatchComp.Props().render)
          }

        (routes | trimSlashes)
          .notFound(redirectToPage(Page.Index)(SetRouteVia.HistoryReplace))
          .renderWith(layout(layoutCfg))
      }
    }

    val crumbSep = <.span(*.topNavBreadcrumbSep, "/")

    def layout(layoutCfg: LayoutCfg)(ctl: RouterCtl, res: Resolution[Page]): VdomElement = {

      def breadcrumb(path: String): TagMod =
        path
          .dropWhile { case '#' | '/' => true; case _ => false }.split('/')
          .iterator.zipWithIndex.toTagMod { case (frag, i) =>
            val x = <.span(frag)
            if (i == 0) x else TagMod(crumbSep, frag)
          }

      def topNav(name: TagMod, tm: TagMod): VdomElement =
        <.div(
          *.topNav,
          ctl.link(Page.Index)("Home"),
          crumbSep,
          name)

      res.page match {
        case Page.Index =>
          layoutCfg topPage res.render()

        case Page.Suite(mi) =>
          layoutCfg.suitePage(topNav(breadcrumb(mi.urlPath), _), res.render())

        case Page.Batch(mi) =>
          layoutCfg.batch(topNav(TOC.batchMode, _), res.render())
      }
    }

    object TOC {

      val batchMode = "Batch mode"

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
            case s: MenuFolder2 => <.div(<.h3(p.headerStyle, s.name), children(s.children))
            case s: MenuSuite2  => p.router.link(Page.Suite(s))(s.suite.name)
            case s: MenuBatch2  => p.router.link(Page.Batch(s))(batchMode)
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
