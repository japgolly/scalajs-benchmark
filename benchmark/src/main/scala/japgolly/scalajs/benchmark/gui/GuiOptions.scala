package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.Suite
import japgolly.scalajs.benchmark.engine.Progress
import scala.scalajs.LinkingInfo

final case class GuiOptions(defaultSuiteResultsFormat: SuiteResultsFormat,
                            suiteResultsFormats      : Seq[SuiteResultsFormat],
                            resultFilenameWithoutExt : GuiOptions.FilenameFormat,
                            batchModeFormats         : Map[SuiteResultsFormat.Text, Enabled],
                            allowBatchMode           : Boolean,
                           ) {

  assert(
    suiteResultsFormats.contains(defaultSuiteResultsFormat),
    s"The default format [${defaultSuiteResultsFormat.label}] isn't in the list of choices ${suiteResultsFormats.map(_.label).mkString("[", ", ", "]")}")
}

object GuiOptions {

  type FilenameFormat = (Suite[_], Progress[_]) => String

  val default: GuiOptions =
    apply(
      defaultSuiteResultsFormat = SuiteResultsFormat.Table,
      suiteResultsFormats       = SuiteResultsFormat.builtIn,
      resultFilenameWithoutExt  = defaultFilenameFormat,
      batchModeFormats          = SuiteResultsFormat.builtInBatch,
      allowBatchMode            = true,
    )

  // Access this via `GuiOptions.default.resultFilenameWithoutExt`
  private def defaultFilenameFormat: FilenameFormat =
    (s, p) => {
      val mode =
        if (LinkingInfo.developmentMode)
          "fastopt-"
        else
          ""
      s"sjsbm-${s.filenameFriendlyName}-${mode}${p.timestampTxt}"
    }
}
