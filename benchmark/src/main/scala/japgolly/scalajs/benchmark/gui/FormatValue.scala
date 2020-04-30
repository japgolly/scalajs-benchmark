package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.Stats
import japgolly.scalajs.react.vdom.html_<^._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scalacss.ScalaCssReact._

/** Format for a single value.
  *
  * Eg. 32.456 sec
  */
final case class FormatValue[-I](getDouble: I => Option[Double], render: I => VdomElement, toText: I => String) {
  def cmap[A](f: A => I): FormatValue[A] =
    FormatValue(getDouble compose f, render compose f, toText compose f)
}

object FormatValue {

  def number(dp: Int): FormatValue[Double] = {
    val fmt = s"%.${dp}f"
    FormatValue(Some.apply,
      d => <.div(
        Styles.Suite.numericResult,
        Util.addThousandSeps(fmt format d)),
      fmt.format(_)
    )
  }

  def optionalNumber(dp: Int, default: VdomElement, defaultText: String): FormatValue[Option[Double]] = {
    val n = number(dp)
    FormatValue(identity, {
      case Some(d) => n render d
      case None    => default
    }, {
      case Some(d) => n toText d
      case None    => defaultText
    })
  }

  def duration(getUnits: FiniteDuration => Double, dp: Int): FormatValue[Duration] =
    optionalNumber(dp, <.span("∞"), "∞").cmap {
      case f: FiniteDuration => Some(getUnits(f))
      case _                 => None
    }

  def averageDuration(getUnits: FiniteDuration => Double, dp: Int): FormatValue[Stats] =
    duration(getUnits, dp).cmap(_.average)

  def error(getUnits: FiniteDuration => Double, dp: Int): FormatValue[Stats] =
    duration(getUnits, dp).cmap(_.marginOfError)

  val Integer = number(0).cmap[Int](_.toDouble)
}

