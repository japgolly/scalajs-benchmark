package demo

import japgolly.scalajs.benchmark.engine.ScalaJsInfo
import japgolly.scalajs.benchmark.gui._
import org.scalajs.dom.experimental.URLSearchParams
import org.scalajs.dom.{document, window}
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.Try

object Main {

  @JSExportTopLevel("main")
  def main(): Unit = {

    val tgt = document.getElementById("body")

    val guiOptions = GuiOptions.default.copy(
      resultFilenameWithoutExt = filenameFormat,
    )

    BenchmarkGUI.renderMenu(tgt, layoutConfig = configureLayout, guiOptions = guiOptions)(
      suites.example.Examples.all,
      suites.cats.all,
      suites.scala.all,
      suites.scalajs.all,
      suites.scalaz.all,
      suites.shootouts.all,
    )
  }

  // Append ?machine=xxx to include xxx in filenames
  private val machineName: Option[String] =
    Try {
      val p = new URLSearchParams(window.location.search)
      Option(p.get("machine")).map(_.trim).filter(_.nonEmpty).map(FilenameCtx.normalise)
    }.toOption.flatten

  /* Customise output filenames
   * Optional, of course.
   */
  private def filenameFormat(ctx: FilenameCtx[_]): String = {
    val machine = machineName.fold("")(_ + "-")
    val scala = Libraries.Scala.version
    val sjs   = ScalaJsInfo.version // includes -fastopt
    s"sjsbm-${machine}${ctx.name}-scala_$scala-sjs_$sjs-${ctx.timestampTxt}"
  }

  /* Customise the layout slightly.
   * Optional, of course.
   */
  import japgolly.scalajs.react.vdom.html_<^._
  import demo.Util._

  private def configureLayout: LayoutConfig = {
    def toc(view: VdomElement): VdomElement =
      <.main(
        <.h1(
          ^.marginBottom := "0.2em",
          "Benchmark Collection"),
        <.div(
          ^.marginBottom := "0.5ex",
          ^.fontSize := "0.8em",
          ^.fontFamily := "monospace",
          s"Generated with ${Libraries.ScalaJs.fullName}."),
        <.div(
          linkToSource(sourceFilename)(^.textAlign.left)),
        <.main(
          ^.marginTop := "2.6em",
          view))
    LayoutConfig.default.copy(toc = toc)
  }
}
