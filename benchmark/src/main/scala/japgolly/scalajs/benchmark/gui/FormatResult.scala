package japgolly.scalajs.benchmark.gui

import java.util.concurrent.TimeUnit
import japgolly.scalajs.benchmark.engine.Stats
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

  def duration(header: String, lowerIsBetter: Boolean, getUnits: FiniteDuration => Double, scoreDP: Int, errorDP: Int): FormatResult =
    FormatResult(
      header,
      FormatValue.averageDuration(getUnits, scoreDP),
      FormatValue.error          (getUnits, errorDP),
      lowerIsBetter)

  def opsPerT(t: TimeUnit, scoreDP: Int, errorDP: Int): FormatResult = {
    val one = FiniteDuration(1, t)
    duration("ops/" + abbrev(t), false, one / _, scoreDP, errorDP)
  }

  def timePerOp(t: TimeUnit, scoreDP: Int, errorDP: Int): FormatResult =
    duration(abbrev(t) + "/op", true, getUnits(t), scoreDP, errorDP)

  val OpsPerSec   = opsPerT(TimeUnit.SECONDS, 1, 0)
  val SecPerOp2   = timePerOp(TimeUnit.SECONDS, 2, 2)
  val SecPerOp3   = timePerOp(TimeUnit.SECONDS, 3, 3)
  val MillisPerOp = timePerOp(TimeUnit.MILLISECONDS, 1, 1)
  val MicrosPerOp = timePerOp(TimeUnit.MICROSECONDS, 0, 0)

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
