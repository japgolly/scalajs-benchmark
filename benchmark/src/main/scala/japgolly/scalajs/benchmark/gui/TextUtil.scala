package japgolly.scalajs.benchmark.gui

import scala.scalajs.js
import scala.scalajs.js.|

object TextUtil {

  private val numberFmt = """^-?(\d[,.]?)+(?:[,.]\d+)?$""".r.pattern

  def formatTable(rows: Iterable[Vector[String]],
                  gap : Int => String = _ => "  "): String = {
    if (rows.isEmpty)
      ""
    else {
      val maxCols = rows.iterator.map(_.length).max
      if (maxCols == 0)
        ""
      else {
        val rightAlign =
          (0 until maxCols)
            .iterator
            .map { i =>
              rows.iterator.drop(1).map(_.lift(i).getOrElse("")).exists(numberFmt.matcher(_).matches)
            }
            .toVector

        val rowFmt = (0 until maxCols)
          .iterator
          .map { i =>
            val maxLen = rows.iterator.map(_.lift(i).fold(0)(_.length)).max.max(1)
            val fmt =
              if (rightAlign(i))
                "%" + maxLen + "s"
              else
                "%-" + maxLen + "s"
            if (i == maxCols - 1)
              fmt
            else
              fmt + gap(i)
          }
          .mkString("")

        rows.iterator.map { cells =>
          rowFmt.format((0 until maxCols).iterator.map(i => cells.lift(i).getOrElse("")).toSeq: _*)
        }.mkString("\n")
      }
    }
  }

  def formatCSV(rows: Iterable[Vector[String]]): String = {
    def quote(s: String): String = {
      val needQuote = s.exists {
        case ' ' | ',' | '"' => true
        case c               => c < 32
      }
      if (needQuote)
        "\"" + s.replace("\"", "\"\"") + "\""
      else
        s
    }
    rows
      .iterator
      .map(_.iterator.map(quote).mkString(","))
      .mkString("\n")
  }

  def prettyPrintNumber(d: Int | Double): String =
    try
      d.asInstanceOf[js.Dynamic].toLocaleString().asInstanceOf[String]
    catch {
      case _: Throwable => d.toString
    }

  def prettyPrintNumber(d: Double, maxDP: Int): String =
    try
      d.asInstanceOf[js.Dynamic].toLocaleString(
        js.undefined,
        js.Dynamic.literal(maximumFractionDigits = maxDP)
      ).asInstanceOf[String]
    catch {
      case _: Throwable => s"%.${maxDP}f".format(d)
    }

  def removeTrailingZeros(str: String): String =
    str.replaceFirst("0+$", "").replaceFirst("\\.$", "")
}
