package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.{IterationStats, Stats, TimeUtil}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, FiniteDuration}

/** Format for the result of a single benchmark.
  *
  * Eg. ops/sec: 2058.8 ± 8.1
  *
  * @param score Formatter for the score itself.
  * @param scoreError Formatter for the error in (score ± error).
  */
final case class BmResultFormat(header          : String,
                                score           : ValueFormat[Stats],
                                scoreError      : ValueFormat[Stats],
                                scoreConfidence1: ValueFormat[Stats],
                                scoreConfidence2: ValueFormat[Stats],
                                lowerIsBetter   : Boolean) {

  def higherIsBetter = !lowerIsBetter

  def whichIsBetter: String =
    (if (lowerIsBetter) "lower" else "higher") + " is better"

  val graphHeader: String =
    s"$header ($whichIsBetter)"
}

object BmResultFormat {

  val OpsPerSec   = opsPerTime(TimeUnit.SECONDS, 0, 0)
  val OpsPerSec3  = opsPerTime(TimeUnit.SECONDS, 3, 3)
  val SecPerOp2   = timePerOp(TimeUnit.SECONDS, 2, 3)
  val SecPerOp3   = timePerOp(TimeUnit.SECONDS, 3, 3)
  val MillisPerOp = timePerOp(TimeUnit.MILLISECONDS, 3, 3)
  val MicrosPerOp = timePerOp(TimeUnit.MICROSECONDS, 3, 3)

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
    fd => f(TimeUtil.toMs(fd))
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

  def duration(header       : String,
               lowerIsBetter: Boolean,
               getUnits     : FiniteDuration => Double,
               scoreDP      : Int,
               errorDP      : Int): BmResultFormat = {
    val scoreError = ValueFormat.duration(getUnits, errorDP).contramap(TimeUtil.fromMs)
    BmResultFormat(
      header           = header,
      score            = ValueFormat.duration(getUnits, scoreDP).contramap(TimeUtil.fromMs).contramap(_.score),
      scoreError       = scoreError.contramap(_.scoreError),
      scoreConfidence1 = scoreError.contramap(_.scoreConfidence._1),
      scoreConfidence2 = scoreError.contramap(_.scoreConfidence._2),
      lowerIsBetter    = lowerIsBetter)
    }

  def opsPerTime(t: TimeUnit, scoreDP: Int, errorDP: Int): BmResultFormat = {
    val oneUnitAsMs = TimeUtil.toMs(FiniteDuration(1, t))
    val scoreError  = ValueFormat.number(errorDP)

    var prevInverse: (Stats, Stats) = null
    def inverse(s: Stats) = {
      if ((prevInverse ne null) && (prevInverse._1 eq s))
        prevInverse._2
      else {
        val i = s.modifyMeans(oneUnitAsMs / _)
        prevInverse = (s, i)
        i
      }
    }

    BmResultFormat(
      header           = "ops/" + abbrev(t),
      score            = ValueFormat.number(scoreDP).contramap(inverse(_).score),
      scoreError       = scoreError.contramap(inverse(_).scoreError),
      scoreConfidence1 = scoreError.contramap(inverse(_).scoreConfidence._1),
      scoreConfidence2 = scoreError.contramap(inverse(_).scoreConfidence._2),
      lowerIsBetter    = false)
  }

  def timePerOp(t: TimeUnit, scoreDP: Int, errorDP: Int): BmResultFormat =
    duration(abbrev(t) + "/op", true, getUnits(t), scoreDP, errorDP)

  def chooseTimePerOp(ctx: Ctx): BmResultFormat =
    chooseTimePerOp(ctx.minDur)

  def chooseTimePerOp(minDur: Duration): BmResultFormat =
    if (minDur.toMicros < 1000)
      MicrosPerOp
    else if (minDur.toMillis < 1000)
      MillisPerOp
    else if (minDur.toSeconds < 10)
      SecPerOp3
    else
      SecPerOp2

  final case class Ctx(minDur: Duration, maxDur: Duration)

  type DynamicMultiple = Ctx => Vector[BmResultFormat]

  object DynamicMultiple {
    val default: DynamicMultiple =
      ctx => {
        val mainFmt = chooseTimePerOp(ctx)
        Vector(mainFmt, BmResultFormat.OpsPerSec)
      }
  }
}
