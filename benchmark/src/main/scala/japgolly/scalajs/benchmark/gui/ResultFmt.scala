package japgolly.scalajs.benchmark.gui

import java.util.concurrent.TimeUnit

import japgolly.scalajs.benchmark.engine.Stats
import japgolly.scalajs.react.vdom.html_<^._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scalacss.ScalaCssReact._

/**
  * Format for a single value.
  *
  * Eg. 32.456 sec
  */
final case class ValueFmt[-I](getDouble: I => Option[Double], render: I => VdomElement, toText: I => String) {
  def cmap[A](f: A => I): ValueFmt[A] =
    ValueFmt(getDouble compose f, render compose f, toText compose f)
}

object ValueFmt {

  def number(dp: Int): ValueFmt[Double] = {
    val fmt = s"%.${dp}f"
    ValueFmt(Some.apply,
      d => <.div(
        Styles.Suite.numericResult,
        Util.addThousandSeps(fmt format d)),
      fmt.format(_)
    )
  }

  def optionalNumber(dp: Int, default: VdomElement, defaultText: String): ValueFmt[Option[Double]] = {
    val n = number(dp)
    ValueFmt(identity, {
      case Some(d) => n render d
      case None    => default
    }, {
      case Some(d) => n toText d
      case None    => defaultText
    })
  }

  def duration(getUnits: FiniteDuration => Double, dp: Int): ValueFmt[Duration] =
    optionalNumber(dp, <.span("∞"), "∞").cmap {
      case f: FiniteDuration => Some(getUnits(f))
      case _                 => None
    }

  def averageDuration(getUnits: FiniteDuration => Double, dp: Int): ValueFmt[Stats] =
    duration(getUnits, dp).cmap(_.average)

  def error(getUnits: FiniteDuration => Double, dp: Int): ValueFmt[Stats] =
    duration(getUnits, dp).cmap(_.marginOfError)

  val Integer = number(0).cmap[Int](_.toDouble)
}

/**
  * Format for a result derived from [[Stats]].
  *
  * Eg. ops/sec: 2058.8 ± 8.1
  *
  * @param score Formatter for the score itself.
  * @param error Formatter for the error in (score ± error).
  */
final case class ResultFmt(header: String, score: ValueFmt[Stats], error: ValueFmt[Stats], lowerIsBetter: Boolean) {
  def higherIsBetter = !lowerIsBetter

  def whichIsBetter: String =
    (if (lowerIsBetter) "lower" else "higher") + " is better"

  val graphHeader: String =
    s"$header ($whichIsBetter)"
}

object ResultFmt {

  def abbrev(t: TimeUnit): String =
    t match {
      case TimeUnit.NANOSECONDS  => "ns"
      case TimeUnit.MICROSECONDS => "μs"
      case TimeUnit.MILLISECONDS => "ms"
      case TimeUnit.SECONDS      => "s"
      case TimeUnit.MINUTES      => "m"
      case TimeUnit.HOURS        => "d"
      case TimeUnit.DAYS         => "hr"
    }

  def getUnits(t: TimeUnit): FiniteDuration => Double =
    t match {
      case TimeUnit.NANOSECONDS  => _.toNanos.toDouble
      case TimeUnit.MICROSECONDS => _.toNanos.toDouble / 1000.0
      case TimeUnit.MILLISECONDS => _.toNanos.toDouble / 1000000.0
      case TimeUnit.SECONDS      => _.toMicros.toDouble / 1000000.0
      case TimeUnit.MINUTES      => _.toMillis.toDouble / 60000.0
      case TimeUnit.HOURS        => _.toMillis.toDouble / 3660000.0
      case TimeUnit.DAYS         => _.toSeconds.toDouble / (3660 * 24)
    }

  def duration(header: String, lowerIsBetter: Boolean, getUnits: FiniteDuration => Double, scoreDP: Int, errorDP: Int): ResultFmt =
    ResultFmt(
      header,
      ValueFmt.averageDuration(getUnits, scoreDP),
      ValueFmt.error          (getUnits, errorDP),
      lowerIsBetter)

  def opsPerT(t: TimeUnit, scoreDP: Int, errorDP: Int): ResultFmt = {
    val one = FiniteDuration(1, t)
    duration("ops/" + abbrev(t), false, one / _, scoreDP, errorDP)
  }

  def timePerOp(t: TimeUnit, scoreDP: Int, errorDP: Int): ResultFmt =
    duration(abbrev(t) + "/op", true, getUnits(t), scoreDP, errorDP)

  val OpsPerSec   = opsPerT(TimeUnit.SECONDS, 1, 0)
  val SecPerOp    = timePerOp(TimeUnit.SECONDS, 2, 1)
  val MillisPerOp = timePerOp(TimeUnit.MILLISECONDS, 1, 1)
  val MicrosPerOp = timePerOp(TimeUnit.MICROSECONDS, 0, 0)
}
