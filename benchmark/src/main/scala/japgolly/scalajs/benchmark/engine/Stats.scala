package japgolly.scalajs.benchmark.engine

import scala.concurrent.duration._
import scala.scalajs.js

final case class Stats(rawData: Vector[Vector[FiniteDuration]], engineOptions: EngineOptions) {

  override def toString() = {
    def toOpsPerSec(d: FiniteDuration): Double =
      TimeUtil.toMs(d) * 1000 / 1000000L.toDouble
    def fmtD(d: Duration): String = d match {
      case f: FiniteDuration => (TimeUtil.toMs(f) * 1000).toInt.toString + "μs"
      case _                 => d.toString
    }
    val tot = "%0.3f sec".format(toOpsPerSec(totalTime))
    s"${fmtD(score)} ± ${fmtD(scoreError)} /op ($samples runs, Σ $tot)"
  }

  lazy val isolatedBatches: Vector[Stats] = {
    val e = Vector.empty[Vector[FiniteDuration]]
    rawData.map { batch =>
      Stats(e :+ batch, engineOptions)
    }
  }

  val times =
    rawData.flatten

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
    StatMath(times.map(TimeUtil.toMs))

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
      TimeUtil.fromMs(meanErrorMsAt(confidence))

  def getConfidenceIntervalAt(confidence: Double): (Duration, Duration) =
    if (samples <= 2)
      (Duration.Undefined, Duration.Undefined)
    else {
      val meanErr = meanErrorMsAt(confidence)
      (TimeUtil.fromMs(statMathMs.mean - meanErr), TimeUtil.fromMs(statMathMs.mean + meanErr))
    }

  val score           = average
  val scoreError      = getMeanErrorAt(0.999)
  val scoreConfidence = getConfidenceIntervalAt(0.999)
}

object Stats {

  private[engine] class Mutable {
    private val batches      = new js.Array[js.Array[FiniteDuration]]
    private var curBatch     = new js.Array[FiniteDuration]
    private var curBatchTime = Duration.Zero

    def add(d: FiniteDuration): Unit = {
      curBatch.push(d)
      curBatchTime += d
    }

    def totalBatchTime() =
      curBatchTime

    def endBatch(): Unit = {
      batches.push(curBatch)
      curBatch = new js.Array[FiniteDuration]
      curBatchTime = Duration.Zero
    }

    /** Make sure you call [[endBatch()]] before calling this. */
    def result(): Vector[Vector[FiniteDuration]] =
      Vector.tabulate(batches.length)(batches(_).toVector)
  }

}
