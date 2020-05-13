package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark.vendor.JStat

object StatMath {
  def tDistributionInverseCumulativeProbability(df: Int, p: Double): Double =
    JStat.studentt.inv(p, df)
}

final case class StatMath(sample: Iterable[Double]) {
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

//  /** 2σ. this ± [[mean]] = 95.45% percent of distribution. */
//  val sigma2 = stddev * 2

//  /** [[sigma2]] as a percentage relative to the mean. */
//  val relSigma2 = (sigma2 / mean) * 100

//  /** 3σ. this ± [[mean]] = 99.73% percent of distribution. */
//  val sigma3 = stddev * 3

//  /** [[sigma3]] as a percentage relative to the mean. */
//  val relSigma3 = (sigma3 / mean) * 100
}