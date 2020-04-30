package japgolly.scalajs.benchmark.gui

/** Format for a number of results.
  */
sealed abstract class FormatResults(final val label: String)

object FormatResults {

  case object Table extends FormatResults("Table")
  case object Text  extends FormatResults("Text")

  def default: FormatResults =
    Table

  val all: Vector[FormatResults] =
    Vector(Table, Text)
}

