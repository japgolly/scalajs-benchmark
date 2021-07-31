package demo

import japgolly.scalajs.react.vdom.html_<^._
import sourcecode.FileName

object Util {

  def sourceFilename(implicit f: FileName): String =
    f.value

  def linkToSource(filename: String) = {
    val url = filename.replaceFirst("^.+?/demo/", "https://github.com/japgolly/scalajs-benchmark/blob/gh-pages/demo/")
    val name = url.replaceFirst("^.+?/(?=demo/)", "").replaceFirst("^.+/(?=suites/)", "…/")
    <.div(^.textAlign.right, ^.fontSize := "0.85em",
      <.span(^.color := "#333", "Source: "),
      <.a(^.fontFamily := "monospace", ^.color := "#33a",
        ^.href := url, name))
  }

}
