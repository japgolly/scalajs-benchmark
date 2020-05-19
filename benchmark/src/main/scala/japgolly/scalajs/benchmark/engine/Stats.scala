package japgolly.scalajs.benchmark.engine

import scala.scalajs.js

final case class Stats(rawData: Vector[IterationStats]) {

  def map(f: IterationStats => IterationStats): Stats =
    Stats(rawData.map(f))

  def modifyMeans(f: Double => Double): Stats =
    map(_.modifyMean(f))

  lazy val isolatedBatches: Vector[Stats] = {
    val e = Vector.empty[IterationStats]
    Vector.tabulate(rawData.length) { i =>
      val batch = rawData(i)
      Stats(e :+ batch)
    }
  }

  private val times: js.Array[Double] = new js.Array[Double]
  private var _sum = 0.0
  for (i <- rawData) {
    _sum += i.mean
    times.push(i.mean)
  }

  val sum: Double =
    _sum

  def samples: Int =
    times.length

  val average: Double =
    if (times.isEmpty)
      0
    else
      sum / samples

  private lazy val statMathMs =
    StatMath(times)

  private def meanErrorMsAt(confidence: Double): Double = {
    val df = samples - 1
    val p = 1 - (1 - confidence) / 2
    val a = StatMath.tDistributionInverseCumulativeProbability(df = df, p = p)
    a * statMathMs.sem
  }

  def getMeanErrorAt(confidence: Double): Double =
    if (samples <= 2)
      Double.NaN
    else
      meanErrorMsAt(confidence)

  def getConfidenceIntervalAt(confidence: Double): (Double, Double) =
    if (samples <= 2)
      (Double.NaN, Double.NaN)
    else {
      val meanErr = meanErrorMsAt(confidence)
      val low = (statMathMs.mean - meanErr).max(0)
      val high = statMathMs.mean + meanErr
      (low, high)
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
