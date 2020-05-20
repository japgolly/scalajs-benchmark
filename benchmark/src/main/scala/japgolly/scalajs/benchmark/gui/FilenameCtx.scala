package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.Suite
import japgolly.scalajs.benchmark.engine.Progress

final case class FilenameCtx[P](folderPath: Seq[String],
                                suite     : Suite[P],
                                progress  : Progress[P]) {
  import FilenameCtx.normalise

  lazy val nameParts: Seq[String] =
    folderPath :+ suite.name

  lazy val name: String =
    nameParts.iterator.map(normalise).mkString("-")

  def timestampTxt: String =
    progress.timestampTxt
}

object FilenameCtx {

  def normalise(s: String): String =
    s.replaceAll("[ .-]", "_")
}