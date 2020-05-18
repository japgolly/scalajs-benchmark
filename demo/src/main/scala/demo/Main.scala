package demo

import japgolly.scalajs.benchmark.gui._
import org.scalajs.dom.document
import scala.scalajs.js.annotation.JSExportTopLevel

object Main {

  @JSExportTopLevel("main")
  def main(): Unit = {

    // import concurrent.duration._
    // import japgolly.scalajs.benchmark.engine.Options
    // val opts = Options.Default.copy(minRuns = 1000, minTime = 0.millis)

    val tgt = document.getElementById("body")

    BenchmarkGUI.renderMenu(tgt, layoutConfig = configureLayout)(
      suites.example.Examples.all,
      suites.cats.all,
      suites.scala.all,
      suites.scalaz.all,
      suites.shootouts.all,
    )
  }

  /* Customise the layout slightly.
   *
   * Optional, of course.
   */
  import japgolly.scalajs.react.vdom.html_<^._
  import demo.Util._

  def configureLayout: LayoutConfig = {
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
