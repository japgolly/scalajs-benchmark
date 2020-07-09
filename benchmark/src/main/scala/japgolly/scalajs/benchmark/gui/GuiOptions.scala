package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.TimeUtil
import scala.scalajs.{LinkingInfo, js}

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

  private val mode: String =
    if (LinkingInfo.developmentMode)
      "fastopt-"
    else
      ""

  // Access this via `GuiOptions.default.resultFilenameWithoutExt`
  private def defaultFilenameFormat(ctx: FilenameCtx[_]): String =
    s"sjsbm-${ctx.name}-${mode}${ctx.timestampTxt}"

  def batchResultFilenameFormat(date: js.Date): String =
    s"sjsbm-${mode}${TimeUtil.timestampStrFromJsDate(date)}"
}
