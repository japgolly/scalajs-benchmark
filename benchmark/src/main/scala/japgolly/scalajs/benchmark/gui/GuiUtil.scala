package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.TimeUtil
import japgolly.scalajs.benchmark.vendor.FileSaver
import japgolly.scalajs.react.Callback
import monocle.{Lens, Optional}
import org.scalajs.dom.{Blob, BlobPropertyBag}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.|

object GuiUtil {

  def formatETA(eta: FiniteDuration): String =
    formatETA(TimeUtil.toMs(eta))

  def formatETA(ms: Double): String = {
    val sec = ms / 1000 + 0.5 // adding 0.5 for rounding
    val min = sec / 60
    val hr  = min / 60
    s"%d:%02d:%02d".format(hr.toInt, (min % 60).toInt, (sec % 60).toInt)
  }

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
        js.Dynamic.literal(minimumFractionDigits = maxDP, maximumFractionDigits = maxDP)
      ).asInstanceOf[String]
    catch {
      case _: Throwable => s"%.${maxDP}f".format(d)
    }

  def removeTrailingZeros(str: String): String =
    str.replaceFirst("0+$", "").replaceFirst("\\.$", "")

  def vectorIndex[A](idx: Int): Lens[Vector[A], A] =
    Lens[Vector[A], A](_(idx))(a => _.patch(idx, a :: Nil, 1))

  def unsafeNarrowLens[A, B <: A: ClassTag]: Lens[A, B] =
    Lens[A, B]({
      case b: B => b
      case a => throw new RuntimeException("Invalid subtype: " + a)
    })(b => _ => b)

  def optionalToLens[S, A](o: Optional[S, A])(default: => A): Lens[S, A] =
    Lens[S, A](o.getOption(_).getOrElse(default))(o.replace)

  def saveFile(text: String, filename: String, mimeType: String): Callback =
    Callback {
      val body = js.Array[js.Any](text)
      val mime = new BlobPropertyBag { `type` = mimeType + ";charset=utf-8" }
      val blob = new Blob(body, mime)
      FileSaver.saveAs(blob, filename)
    }

  def showFiniteDuration(d: Duration): String =
    d.toString
      .replaceFirst("milliseconds?", "ms")
      .replaceFirst("seconds?", "sec")
      .replaceFirst("minutes?", "min")
      .replace("0 days", "0 sec")
}
