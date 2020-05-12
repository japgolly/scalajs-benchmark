package japgolly.scalajs.benchmark.gui

import java.util.concurrent.TimeUnit
import japgolly.scalajs.benchmark.engine.{DurationUtil, Stats}
import scala.concurrent.duration.{Duration, FiniteDuration}

/** Format for a result derived from [[Stats]].
  *
  * Eg. ops/sec: 2058.8 ± 8.1
  *
  * @param score Formatter for the score itself.
  * @param error Formatter for the error in (score ± error).
  */
final case class FormatResult(header       : String,
                              score        : FormatValue[Stats],
                              error        : FormatValue[Stats],
                              lowerIsBetter: Boolean) {

  def higherIsBetter = !lowerIsBetter

  def whichIsBetter: String =
    (if (lowerIsBetter) "lower" else "higher") + " is better"

  val graphHeader: String =
    s"$header ($whichIsBetter)"
}

object FormatResult {

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

  def getUnits(t: TimeUnit): FiniteDuration => Double = {
    val f = getUnitsFromMs(t)
    fd => f(DurationUtil.toMs(fd))
  }

  private def getUnitsFromMs(t: TimeUnit): Double => Double =
    t match {
      case TimeUnit.NANOSECONDS  => _ * 1000000
      case TimeUnit.MICROSECONDS => _ * 1000
      case TimeUnit.MILLISECONDS => identity
      case TimeUnit.SECONDS      => _ / 1000
      case TimeUnit.MINUTES      => _ / 60000
      case TimeUnit.HOURS        => _ / 3660000
      case TimeUnit.DAYS         => _ / 3660000 / 24
    }

  def duration(header: String, lowerIsBetter: Boolean, getUnits: FiniteDuration => Double, scoreDP: Int, errorDP: Int): FormatResult =
    FormatResult(
      header,
      FormatValue.score     (getUnits, scoreDP),
      FormatValue.scoreError(getUnits, errorDP),
      lowerIsBetter)

  def opsPerT(t: TimeUnit, scoreDP: Int, errorDP: Int): FormatResult = {
    val one = FiniteDuration(1, t)
    duration("ops/" + abbrev(t), false, one / _, scoreDP, errorDP)
  }

  def timePerOp(t: TimeUnit, scoreDP: Int, errorDP: Int): FormatResult =
    duration(abbrev(t) + "/op", true, getUnits(t), scoreDP, errorDP)

  val OpsPerSec   = opsPerT(TimeUnit.SECONDS, 0, 0)
  val SecPerOp2   = timePerOp(TimeUnit.SECONDS, 2, 3)
  val SecPerOp3   = timePerOp(TimeUnit.SECONDS, 3, 3)
  val MillisPerOp = timePerOp(TimeUnit.MILLISECONDS, 3, 3)
  val MicrosPerOp = timePerOp(TimeUnit.MICROSECONDS, 3, 3)

  def choose(minDur: Duration): FormatResult =
    if (minDur.toMicros < 1000)
      MicrosPerOp
    else if (minDur.toMillis < 1000)
      MillisPerOp
    else if (minDur.toSeconds < 10)
      SecPerOp3
    else
      SecPerOp2
}
