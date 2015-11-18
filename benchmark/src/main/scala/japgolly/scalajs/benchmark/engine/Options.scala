package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import scala.concurrent.duration._

object Options {

  object Defaults {
    val InitialDelay   = 4.millis
    val OutlierTrimPct = 0.01
    val MinRuns        = 10000
    val MinTime        = 1.second
    val MaxTime        = 10.second

    // Ensure benchmarks don't start before chart animation finishes
    def delay: FiniteDuration = {
      val chartTimeSec = Chart.defaults.global.animationSteps / 60.0
      val delaySec     = chartTimeSec * 1.2 // some buffer
      val delayMicro   = delaySec * 1000000.0
      delayMicro.toInt.micros
    }
  }

  def default: Options =
    Options()
}

final case class Options(
  clock         : Clock          = Clock.Default,
  initialDelay  : FiniteDuration = Options.Defaults.InitialDelay,
  delay         : FiniteDuration = Options.Defaults.delay,
  outlierTrimPct: Double         = Options.Defaults.OutlierTrimPct,
  minRuns       : Int            = Options.Defaults.MinRuns,
  minTime       : FiniteDuration = Options.Defaults.MinTime,
  maxTime       : FiniteDuration = Options.Defaults.MaxTime)
