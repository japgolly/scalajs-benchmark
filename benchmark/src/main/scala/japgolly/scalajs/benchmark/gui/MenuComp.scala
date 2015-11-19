package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.Options
import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.extra.router.{RouterCtl => RouterCtl_, _}
import org.scalajs.dom
import scalacss.ScalaCssReact._
import Styles.{Menu => *}

/**
  * A React component that provides a menu of benchmarking suites, and allows the user to navigate around them.
  */
object MenuComp {

  case class UrlFrag(path: String) extends AnyVal
  object UrlFrag {
    def from(string: String): UrlFrag =
      UrlFrag(string.toLowerCase.replaceAll("[^a-zA-Z0-9-]+", "_"))
  }

  sealed trait MenuItem
  case class MenuSuite(urlFrag: UrlFrag, suite: GuiSuite[_]) extends MenuItem
  case class MenuFolder(name: String, urlFrag: UrlFrag, children: MenuItems) extends MenuItem
  type MenuItems = Traversable[MenuItem]

  implicit def autoLiftGuiSuite(s: GuiSuite[_]): MenuSuite =
    MenuSuite(UrlFrag from s.name, s)

  implicit def autoSoleMenuItem(s: MenuItem): MenuItems =
    s :: Nil

  def folder(name: String)(c: MenuItem*): MenuFolder =
    MenuFolder(name, UrlFrag from name, c)

  def buildRouter(baseUrl: BaseUrl, options: Options = Options.Default)(m1: MenuItems, mn: MenuItems*): Router[_] = {
    val mis = m1.toVector ++ mn.flatten
    val mis2 = Internals.convert(mis)
    val cfg = Internals.routerCfg(mis2, options)
    Router(baseUrl, cfg)
  }

  // ↑ Public stuff
  // ===================================================================================================================
  // ↓ Internals

  private object Internals {

    sealed trait MenuItem2
    type MenuItems2 = Traversable[MenuItem2]
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

    def routerCfg(mis: MenuItems2, options: Options): RouterConfig[Page] = {
      val idx = index(mis)

      RouterConfigDsl[Page].buildConfig { dsl =>
        import dsl._

        val rootRoute: Rule =
          staticRoute(root, None) ~> renderR(rc => TOC.Comp(TOC.Props(mis, rc)))

        val routes =
          idx.foldLeft(rootRoute){ case (q, (path, mi)) =>
            q | staticRoute(path, Some(mi)) ~> render(SuiteComp.Comp(SuiteComp.Props(mi.suite, options)))
          }

        (routes | trimSlashes)
          .notFound(redirectToPage(None)(Redirect.Replace))
          .renderWith(layout)
          .verify(None, idx.valuesIterator.map(Some(_)).toList: _*)
      }
    }

    val crumbSep = <.span(*.topNavBreadcrumbSep, "/")

    def layout(ctl: RouterCtl, res: Resolution[Page]): ReactElement =
      res.page match {
        case None =>
          res.render()

        case Some(mi) =>
          def breadcrumb = mi.urlPath
            .dropWhile { case '#' | '/' => true; case _ => false }.split('/')
            .iterator.zipWithIndex.map[TagMod] { case (frag, i) =>
              val x = <.span(frag)
              if (i == 0) x else TagMod(crumbSep, frag)
            }.reduce(_ + _)

          def topNav =
            <.table(*.topNav,
              <.tbody(
                <.tr(
                  <.td(*.topNavBreadcrumb, breadcrumb),
                  <.td(*.topNavBack, ctl.link(None)("← Back")))))

          <.div(topNav, res.render())
      }

    object TOC {
      case class Props(mis: MenuItems2, rc: RouterCtl)

      class Backend($: BackendScope[Props, Unit]) {
        def render(p: Props) = {

          def children(mis: MenuItems2): ReactTag =
            <.ul(
              mis.toIterator.zipWithIndex.map(x =>
                <.li(^.key := x._2, go(x._1))
              ).toReactNodeArray)

          def go(mi: MenuItem2): ReactTag =
            mi match {
              case s: MenuSuite2  => p.rc.link(Some(s))(s.suite.name)
              case s: MenuFolder2 => <.div(<.h3(s.name), children(s.children))
            }

          children(p.mis)
        }
      }

      val Comp =
        ReactComponentB[Props]("ToC")
          .renderBackend[Backend]
          .build
    }
  }
}
