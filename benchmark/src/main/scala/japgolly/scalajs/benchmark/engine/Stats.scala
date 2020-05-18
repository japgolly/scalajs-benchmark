package japgolly.scalajs.benchmark.engine

import scala.concurrent.duration._
import scala.scalajs.js

final case class Stats(rawData: Vector[IterationStats]) {

  def map(f: Double => Double): Stats =
    Stats(rawData.map(_.map(f)))

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
    val e = Vector.empty[IterationStats]
    Vector.tabulate(rawData.length) { i =>
      val batch = rawData(i)
      Stats(e :+ batch)
    }
  }

  private val times: js.Array[Double] =
    new js.Array[Double]

  for (i <- rawData)
    times.push(i.mean)

  def samples =
    times.length

  val totalTime: FiniteDuration =
    if (times.isEmpty)
      Duration.Zero
    else
      TimeUtil.fromMs(times.sum)

  val average: Duration =
    if (times.isEmpty)
      Duration.Undefined
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
      Duration.Undefined
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

  final class Builder {
    private val iterations   = Vector.newBuilder[IterationStats]
    private var curIteration = new IterationStats.Builder

    def add(d: Double): Unit =
      curIteration.add(d)

    def totalIterationTime() =
      curIteration.totalTime()

    def endIteration(): Unit = {
      iterations += curIteration.result()
      curIteration = new IterationStats.Builder
    }

    /** Make sure you call [[endIteration()]] before calling this. */
    def result(): Stats =
      Stats(iterations.result())
  }

}
