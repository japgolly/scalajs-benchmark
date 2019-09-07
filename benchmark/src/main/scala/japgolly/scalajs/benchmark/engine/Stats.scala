package japgolly.scalajs.benchmark.engine

import scala.concurrent.duration._
import scala.scalajs.js

final case class Stats(times: Vector[FiniteDuration], o: Options) {

  override def toString() = {
    def toOpsPerSec(d: FiniteDuration): Double =
      d.toMicros.toDouble / 1000000L.toDouble
    def fmtD(d: Duration): String =
      d.toMicros.toInt.toString + "μs"
    val tot = "%0.3f sec".format(toOpsPerSec(totalTime))
    s"${fmtD(average)} ± ${fmtD(marginOfError)} ${marginOfErrorRel.toInt}% /op ($runs runs, Σ $tot)"
  }

  def runs =
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
      totalTime / runs

  val statsInMicroSec: StatMath = {
    val a = times.map(_.toMicros.toDouble)
    // org.scalajs.dom.console.log(a.mkString("[", ", ","]"))
    val b = if (a.length < o.outlierTrimIfMin) a else
      StatMath.removeHighOutliers(a, o.outlierTrimPct)
    StatMath(b)
  }

  def marginOfError: FiniteDuration =
    FiniteDuration(statsInMicroSec.sigma2.toLong, MICROSECONDS)

  /** [0,100]% */
  def marginOfErrorRel: Double =
    statsInMicroSec.relSigma2
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
