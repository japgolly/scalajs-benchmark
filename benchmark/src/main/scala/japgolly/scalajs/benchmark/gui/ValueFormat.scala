package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react.vdom.html_<^._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scalacss.ScalaCssReact._

/** Format for a single value.
  *
  * Eg. 32.456 sec
  */
final case class ValueFormat[-I](getDouble   : I => Option[Double],
                                 render      : I => VdomElement,
                                 toDouble    : I => Double,
                                 toTextPretty: I => String,
                                 toTextBasic : I => String) {
  def contramap[A](f: A => I): ValueFormat[A] =
    ValueFormat(
      getDouble compose f,
      render compose f,
      toDouble compose f,
      toTextPretty compose f,
      toTextBasic compose f)
}

object ValueFormat {

  def number(dp: Int): ValueFormat[Double] = {
    val fmt = s"%.${dp}f"
    ValueFormat(
      Some.apply,
      d => <.div(
        Styles.Suite.numericResult,
        GuiUtil.prettyPrintNumber(d, dp)),
      identity,
      GuiUtil.prettyPrintNumber(_, dp),
      fmt.format(_),
    )
  }

  def optionalNumber(dp: Int, default: VdomElement, defaultDouble: Double, defaultText: String): ValueFormat[Option[Double]] = {
    val n = number(dp)
    ValueFormat(
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

  def optionalDouble(dp: Int): ValueFormat[Option[Double]] =
    optionalNumber(
      dp            = dp,
      default       = <.div(Styles.Suite.numericResult, "NaN"),
      defaultDouble = Double.NaN,
      defaultText   = "NaN")

  def duration(getUnits: FiniteDuration => Double, dp: Int): ValueFormat[Duration] =
    optionalDouble(dp).contramap {
      case f: FiniteDuration => Some(getUnits(f))
      case _                 => None
    }

  def durationMs(getUnitsFromMs: Double => Double, dp: Int): ValueFormat[Double] =
    optionalDouble(dp).contramap(ms =>
      if (ms.isFinite)
        Some(getUnitsFromMs(ms))
      else
        None
    )

  val Integer = number(0).contramap[Int](_.toDouble)
}

