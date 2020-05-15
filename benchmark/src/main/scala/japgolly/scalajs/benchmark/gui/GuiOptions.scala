package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.Suite
import japgolly.scalajs.benchmark.engine.Progress
import scala.scalajs.LinkingInfo

final case class GuiOptions(formatResultsDefault    : FormatResults,
                            formatResults           : Seq[FormatResults],
                            resultFilenameWithoutExt: (Suite[_], Progress[_]) => String,
                            allowBatchMode          : Boolean,
                           ) {

  assert(
    formatResults.contains(formatResultsDefault),
    s"The default format [${formatResultsDefault.label}] isn't in the list of choices ${formatResults.map(_.label).mkString("[", ", ", "]")}")
}

object GuiOptions {

  val default: GuiOptions =
    apply(
      formatResultsDefault     = FormatResults.Table,
      formatResults            = FormatResults.builtIn,
      resultFilenameWithoutExt = defaultFilename,
      allowBatchMode           = true,
    )

  // Access this via `default.resultFilenameWithoutExt`
  private def defaultFilename: (Suite[_], Progress[_]) => String =
    (s, p) => {
      val mode =
        if (LinkingInfo.developmentMode)
          "fastopt-"
        else
          ""
      s"sjsbm-${s.filenameFriendlyName}-${mode}${p.timestampTxt}"
    }
}
