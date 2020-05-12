package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark.vendor.JStat

object StatMath {

  /** Remove the largest `pct`% of values. */
  def removeHighOutliers(sample: Vector[Double], pct: Double): Vector[Double] = {
    val t = (sample.length.toDouble * pct).toInt
    if (t > 0) {
      // import scala.math.Ordering.Double.TotalOrdering
      sample.sorted.dropRight(t)
    } else
      sample
  }

  def tDistributionInverseCumulativeProbability(df: Int, p: Double): Double =
    JStat.studentt.inv(p, df)

}

case class StatMath(sample: Iterable[Double]) {
  import Math._

  val size = sample.size

  /** Mean / Average. */
  val mean = sample.sum / size

  /** Sample variance */
  val variance = sample.iterator.map(s => pow(s - mean, 2)).sum / (size - 1)

  /** Standard deviation */
  val stddev = sqrt(variance)

  /** Standard error of the mean. */
  val sem = stddev / sqrt(size)

  /** 2σ. this ± [[mean]] = 95.45% percent of distribution. */
  val sigma2 = stddev * 2

  /** [[sigma2]] as a percentage relative to the mean. */
  val relSigma2 = (sigma2 / mean) * 100

  /** 3σ. this ± [[mean]] = 99.73% percent of distribution. */
  val sigma3 = stddev * 3

  /** [[sigma3]] as a percentage relative to the mean. */
  val relSigma3 = (sigma3 / mean) * 100

  // degrees of freedom
  // private def df = size - 1

  // private def critical = Stats.tDistribution(df)

  // val marginOfError = sem * critical

  // relative margin of error.
  // val rme = (marginOfError / mean) * 100.0

//  org.scalajs.dom.console.log(
//    s"mean: %.2f, var: %.2f, σ: %.2f, 2σ: %.2f, 2σ/μ: %.1f%%\n".format(mean, variance, stddev, sigma2, relSigma2),
//    sample.mkString("[", ", ","]"))
    // scalajs.js.Array[Double](sample.toSeq.sorted.reverse: _*))
}