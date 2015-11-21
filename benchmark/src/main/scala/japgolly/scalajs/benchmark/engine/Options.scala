package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import scala.concurrent.duration._

object Options {

  object Defaults {
    val InitialDelay     = 4.millis
    val OutlierTrimIfMin = 1000
    val OutlierTrimPct   = 0.08
    val MinRuns          = 10000
    val MinTime          = 1.second
    val MaxRuns          = 100000
    val MaxTime          = 12.second

    // Ensure benchmarks don't start before chart animation finishes
    val Delay: () => FiniteDuration = () => {
      val chartTimeSec = Chart.defaults.global.animationSteps / 60.0
      val delaySec     = chartTimeSec * 1.4 // some buffer
      val delayMicro   = delaySec * 1000000.0
      delayMicro.toInt.micros
    }
  }

  val Default: Options =
    Options()
}

final case class Options(
  clock           : Clock                = Clock.Default,
  initialDelay    : FiniteDuration       = Options.Defaults.InitialDelay,
  delay           : () => FiniteDuration = Options.Defaults.Delay,
  outlierTrimIfMin: Int                  = Options.Defaults.OutlierTrimIfMin,
  outlierTrimPct  : Double               = Options.Defaults.OutlierTrimPct,
  minRuns         : Int                  = Options.Defaults.MinRuns,
  minTime         : FiniteDuration       = Options.Defaults.MinTime,
  maxRuns         : Int                  = Options.Defaults.MaxRuns,
  maxTime         : FiniteDuration       = Options.Defaults.MaxTime)
