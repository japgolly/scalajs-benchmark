package japgolly.scalajs.benchmark.engine

import scala.concurrent.duration._
import scala.scalajs.js

/**
  * @param rawData Times in milliseconds per execution, per iteration
  */
final case class Stats(rawData: js.Array[js.Array[Double]], engineOptions: EngineOptions) {

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

  lazy val isolatedBatches: Vector[Stats] =
    Vector.tabulate(rawData.length) { i =>
      val batch = rawData(i)
      Stats(js.Array(batch), engineOptions)
    }

  val times: js.Array[Double] =
    rawData.length match {
      case 0 => new js.Array[Double]
      case 1 => rawData.head
      case _ => rawData.head.concat(rawData.iterator.drop(1).toSeq: _*)
    }


  def samples =
    times.length

  val totalTime: FiniteDuration =
    if (times.isEmpty)
      Duration.Zero
    else
      TimeUtil.fromMs(times.sum)

  val average: Duration =
    if (times.isEmpty)
      Duration.Inf
    else
      totalTime / samples

  private lazy val statMathMs =
    StatMath(times)

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
    private val batches      = new js.Array[js.Array[Double]]
    private var curBatch     = new js.Array[Double]
    private var curBatchTime = 0.0

    def add(d: Double): Unit = {
      curBatch.push(d)
      curBatchTime += d
    }

    def totalBatchTime() =
      curBatchTime

    def endBatch(): Unit = {
      batches.push(curBatch)
      curBatch = new js.Array[Double]
      curBatchTime = 0.0
    }

    /** Make sure you call [[endBatch()]] before calling this. */
    def result(): js.Array[js.Array[Double]] =
      batches
  }

}
