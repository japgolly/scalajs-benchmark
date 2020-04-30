package japgolly.scalajs.benchmark.gui

final case class GuiOptions(formatResultsDefault: FormatResults,
                            formatResults       : Seq[FormatResults]) {

  assert(
    formatResults.contains(formatResultsDefault),
    s"The default format [${formatResultsDefault.label}] isn't in the list of choices ${formatResults.map(_.label).mkString("[", ", ", "]")}")
}

object GuiOptions {

  val default: GuiOptions =
    apply(
      formatResultsDefault = FormatResults.Table,
      formatResults        = FormatResults.builtIn,
    )
}
