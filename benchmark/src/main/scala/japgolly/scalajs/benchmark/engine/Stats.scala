package japgolly.scalajs.benchmark.engine

import scala.concurrent.duration._
import scala.scalajs.js

final case class Stats(times: Vector[FiniteDuration], o: EngineOptions) {

  override def toString() = {
    def toOpsPerSec(d: FiniteDuration): Double =
      DurationUtil.toMs(d) * 1000 / 1000000L.toDouble
    def fmtD(d: Duration): String = d match {
      case f: FiniteDuration => (DurationUtil.toMs(f) * 1000).toInt.toString + "μs"
      case _                 => d.toString
    }
    val tot = "%0.3f sec".format(toOpsPerSec(totalTime))
    s"${fmtD(score)} ± ${fmtD(scoreError)} /op ($samples runs, Σ $tot)"
  }

  def samples =
    times.length

  val totalTime: FiniteDuration =
    if (times.isEmpty)
      Duration.Zero
    else
      times.reduce(_ + _)

  val average: Duration =
    if (times.isEmpty)
      Duration.Inf
    else
      totalTime / samples

  private lazy val statMathMs =
    StatMath(times.map(DurationUtil.toMs))

  private def meanErrorMsAt(confidence: Double): Double = {
    val df = samples - 1
    val p = 1 - (1 - confidence) / 2
    val a = StatMath.tDistributionInverseCumulativeProbability(df = df, p = p)
    a * statMathMs.sem
  }

  def getMeanErrorAt(confidence: Double): Duration =
    if (samples <= 2)
      Duration.Inf
    else
      DurationUtil.fromMs(meanErrorMsAt(confidence))

  def getConfidenceIntervalAt(confidence: Double): (Duration, Duration) =
    if (samples <= 2)
      (Duration.Undefined, Duration.Undefined)
    else {
      val meanErr = meanErrorMsAt(confidence)
      (DurationUtil.fromMs(statMathMs.mean - meanErr), DurationUtil.fromMs(statMathMs.mean + meanErr))
    }

  val score           = average
  val scoreError      = getMeanErrorAt(0.999)
  val scoreConfidence = getConfidenceIntervalAt(0.999)
}

object Stats {

  private[engine] class Mutable {
    var times = new js.Array[FiniteDuration]
    var totalTime = Duration.Zero

    def add(d: FiniteDuration): Unit = {
      times push d
      totalTime += d
    }

    def runs = times.length
  }
}
