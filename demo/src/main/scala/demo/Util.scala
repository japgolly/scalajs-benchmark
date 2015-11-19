package demo

import japgolly.scalajs.react._, vdom.prefix_<^._
import scala.language.experimental.macros

object Util {

  def sourceFilename: String =
    macro japgolly.scalajs.benchmark.macros.UtilMacrosImpl.sourceFilename

  def linkToSource(filename: String) = {
    val name = filename.replaceFirst("^.+/(?=suites/)", "")
    val url = filename.replaceFirst("^.+?/demo/", "https://github.com/japgolly/scalajs-benchmark/blob/master/demo/")
    <.div(^.textAlign.right, ^.fontSize := "0.85em",
      <.span(^.color := "#333", "Source: "),
      <.a(^.fontFamily := "monospace", ^.color := "#33a",
        ^.href := url, name))
  }

}
