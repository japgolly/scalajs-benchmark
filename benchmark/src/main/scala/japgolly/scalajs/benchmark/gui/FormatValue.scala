package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react.vdom.html_<^._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scalacss.ScalaCssReact._

/** Format for a single value.
  *
  * Eg. 32.456 sec
  */
final case class FormatValue[-I](getDouble   : I => Option[Double],
                                 render      : I => VdomElement,
                                 toDouble    : I => Double,
                                 toTextPretty: I => String,
                                 toTextBasic : I => String) {
  def contramap[A](f: A => I): FormatValue[A] =
    FormatValue(
      getDouble compose f,
      render compose f,
      toDouble compose f,
      toTextPretty compose f,
      toTextBasic compose f)
}

object FormatValue {

  def number(dp: Int): FormatValue[Double] = {
    val fmt = s"%.${dp}f"
    FormatValue(
      Some.apply,
      d => <.div(
        Styles.Suite.numericResult,
        TextUtil.prettyPrintNumber(d, dp)),
      identity,
      TextUtil.prettyPrintNumber(_, dp),
      fmt.format(_),
    )
  }

  def optionalNumber(dp: Int, default: VdomElement, defaultDouble: Double, defaultText: String): FormatValue[Option[Double]] = {
    val n = number(dp)
    FormatValue(
      identity,
      {
        case Some(d) => n render d
        case None    => default
      },
      {
        case Some(d) => d
        case None    => defaultDouble
      },
      {
        case Some(d) => n toTextPretty d
        case None    => defaultText
      },
      {
        case Some(d) => n toTextBasic d
        case None    => defaultText
      })
  }

  def duration(getUnits: FiniteDuration => Double, dp: Int): FormatValue[Duration] =
    optionalNumber(
      dp            = dp,
      default       = <.span("NaN"),
      defaultDouble = Double.NaN,
      defaultText   = "NaN")
      .contramap {
        case f: FiniteDuration => Some(getUnits(f))
        case _                 => None
      }

  val Integer = number(0).contramap[Int](_.toDouble)
}

