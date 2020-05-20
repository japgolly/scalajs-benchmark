package japgolly.scalajs.benchmark.gui

import scala.scalajs.LinkingInfo

final case class GuiOptions(defaultSuiteResultsFormat: SuiteResultsFormat,
                            suiteResultsFormats      : Seq[SuiteResultsFormat],
                            bmResultFormats          : BmResultFormat.DynamicMultiple,
                            resultFilenameWithoutExt : GuiOptions.FilenameFormat,
                            batchModeFormats         : Map[SuiteResultsFormat.Text, Enabled],
                            allowBatchMode           : Boolean,
                           ) {

  assert(
    suiteResultsFormats.contains(defaultSuiteResultsFormat),
    s"The default format [${defaultSuiteResultsFormat.label}] isn't in the list of choices ${suiteResultsFormats.map(_.label).mkString("[", ", ", "]")}")
}

object GuiOptions {

  type FilenameFormat = FilenameCtx[_] => String

  val default: GuiOptions =
    apply(
      defaultSuiteResultsFormat = SuiteResultsFormat.Table,
      suiteResultsFormats       = SuiteResultsFormat.builtIn,
      bmResultFormats           = BmResultFormat.DynamicMultiple.default,
      resultFilenameWithoutExt  = defaultFilenameFormat,
      batchModeFormats          = SuiteResultsFormat.builtInBatch,
      allowBatchMode            = true,
    )

  // Access this via `GuiOptions.default.resultFilenameWithoutExt`
  private def defaultFilenameFormat(ctx: FilenameCtx[_]): String = {
    val mode =
      if (LinkingInfo.developmentMode)
        "fastopt-"
      else
        ""
    s"sjsbm-${ctx.name}-${mode}${ctx.timestampTxt}"
  }
}
