package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.react.extra.router.{BaseUrl, Router => SJRRouter}

object GuiBuilder {

  def folder(name: String, urlFrag: UrlFrag = null)(c: MenuItem.NonBatchMode*): MenuItem.Folder = {
    val cs = c.iterator
      .map[((Int, String), MenuItem.NonBatchMode)] {
        case m: MenuItem.Folder => ((0, m.name), m)
        case m: MenuItem.Suite  => ((1, m.suite.name), m)
      }
      .toVector
      .sortBy(_._1)
      .map(_._2)
    folderUnsorted(name, urlFrag)(cs: _*)
  }

  def folderUnsorted(name: String, urlFrag: UrlFrag = null)(c: MenuItem.NonBatchMode*): MenuItem.Folder = {
    val uf = Option(urlFrag).getOrElse(UrlFrag from name)
    MenuItem.Folder(name, uf, c.toVector)
  }

  def router(baseUrl      : BaseUrl,
             layoutConfig : LayoutConfig  = LayoutConfig.default,
             engineOptions: EngineOptions = EngineOptions.default,
             guiOptions   : GuiOptions    = GuiOptions.default)
            (m1: Seq[MenuItem], mn: Seq[MenuItem]*): SJRRouter[_] = {

    var mis = m1.toVector ++ mn.flatten
    if (guiOptions.allowBatchMode)
      mis :+= MenuItem.BatchMode
    val mis2 = convertMenuItems(mis)
    val cfg = Router.config(mis2, layoutConfig, engineOptions, guiOptions)
    SJRRouter(baseUrl, cfg)
  }
  // ===================================================================================================================

  sealed trait MenuItem

  object MenuItem {
    trait NonBatchMode extends MenuItem
    final case class Suite(urlFrag: UrlFrag, suite: GuiSuite[_]) extends NonBatchMode
    final case class Folder(name: String, urlFrag: UrlFrag, children: Vector[NonBatchMode]) extends NonBatchMode
    case object BatchMode extends MenuItem

    implicit def autoLiftGuiSuite(s: GuiSuite[_]): Suite =
      Suite(UrlFrag from s.name, s)

    implicit def autoSoleMenuItem[T](t: T)(implicit f: T => MenuItem): Seq[MenuItem] =
      f(t) :: Nil
  }

  private def convertMenuItems(mis: Seq[MenuItem]): Seq[TableOfContents.Item] = {
    def goNBM(path: String, mi: MenuItem.NonBatchMode): TableOfContents.Item.NonBatchMode = {
      def addFrag(suffix: UrlFrag): String =
        path + suffix.path

      mi match {
        case i: MenuItem.Folder =>
          val prefix = addFrag(i.urlFrag) + "/"
          TableOfContents.Item.Folder(i.name, i.children.map(goNBM(prefix, _)))

        case i: MenuItem.Suite =>
          TableOfContents.Item.Suite(addFrag(i.urlFrag), i.suite)
      }
    }

    def go(path: String, mi: MenuItem): TableOfContents.Item = {
      def addFrag(suffix: UrlFrag): String =
        path + suffix.path

      mi match {
        case i: MenuItem.NonBatchMode =>
          goNBM(path, i)

        case MenuItem.BatchMode =>
          TableOfContents.Item.BatchMode(addFrag(UrlFrag.from("batch-mode"))) // TODO ensure unique
      }
    }

    mis.map(go("#/", _))
  }
}
