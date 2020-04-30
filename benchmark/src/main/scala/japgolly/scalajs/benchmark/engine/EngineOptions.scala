package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import scala.concurrent.duration._

final case class EngineOptions(clock           : Clock,
                               initialDelay    : FiniteDuration,
                               delay           : () => FiniteDuration,
                               outlierTrimIfMin: Int,
                               outlierTrimPct  : Double,
                               minRuns         : Int,
                               minTime         : FiniteDuration,
                               maxRuns         : Int,
                               maxTime         : FiniteDuration,
                              )

object EngineOptions {

  val default: EngineOptions =
    apply(
      clock            = Clock.Default,
      initialDelay     = 4.millis,
      delay            = defaultDelay,
      outlierTrimIfMin = 1000,
      outlierTrimPct   = 0.08,
      minRuns          = 10000,
      minTime          = 1.second,
      maxRuns          = 100000,
      maxTime          = 12.second,
    )

  // Ensure benchmarks don't start before chart animation finishes
  private def defaultDelay: () => FiniteDuration = () => {
    val chartTimeSec = Chart.defaults.global.animationSteps / 60.0
    val delaySec     = chartTimeSec * 1.4 // some buffer
    val delayMicro   = delaySec * 1000000.0
    delayMicro.toInt.micros
  }
}

