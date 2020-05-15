package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark.vendor.JStat
import scala.scalajs.js

object StatMath {
  def tDistributionInverseCumulativeProbability(df: Int, p: Double): Double =
    JStat.studentt.inv(p, df)
}

final case class StatMath(samples: js.Array[Double]) {
  import Math._

  def size = samples.length

  /** Mean / Average. */
  val mean = {
    var sum = 0.0
    var i = samples.length
    while (i > 0) {
      i -= 1
      val d = samples(i)
      sum += d
    }
    sum / size
  }

  /** Sample variance */
  val variance = {
    var sum = 0.0
    var i = samples.length
    while (i > 0) {
      i -= 1
      val d = samples(i) - mean
      val sq = d * d
      sum += sq
    }
    sum / (size - 1)
  }

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