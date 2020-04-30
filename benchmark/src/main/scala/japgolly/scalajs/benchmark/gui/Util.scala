package japgolly.scalajs.benchmark.gui

object Util {

  private val numberFmt = """^-?(\d,?)+(?:\.\d+)?$""".r.pattern

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

  private val addThouRegex = """(\d)(?=(\d\d\d)+(?!\d))""".r

  def addThousandSeps(s: String): String = {
    def go(s: String) = addThouRegex.replaceAllIn(s, "$1,")
    s.indexOf('.') match {
      case n if n >= 0 =>
        val (a,b) = s.splitAt(n)
        go(a) + b
      case _ =>
        go(s)
    }
  }

}
