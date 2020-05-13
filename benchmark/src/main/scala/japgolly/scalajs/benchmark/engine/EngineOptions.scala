package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import scala.concurrent.duration._

/**
  * @param warmupIterationTime None means use [[iterationTime]]
  */
final case class EngineOptions(clock              : Clock,
                               initialDelay       : FiniteDuration,
                               delay              : () => FiniteDuration,
                               warmupIterations   : Int,
                               warmupIterationTime: Option[FiniteDuration],
                               iterations         : Int,
                               iterationTime      : FiniteDuration,
                              ) {

  def actualWarmupIterationTime: FiniteDuration =
    warmupIterationTime.getOrElse(iterationTime)
}

object EngineOptions {

  val default: EngineOptions =
    apply(
      clock               = Clock.Default,
      initialDelay        = 4.millis,
      delay               = defaultDelay,
      warmupIterationTime = None,
      warmupIterations    = 1,
      iterations          = 4,
      iterationTime       = 2.seconds,
    )

  // Ensure benchmarks don't start before chart animation finishes
  private def defaultDelay: () => FiniteDuration = () => {
    val chartTimeSec = Chart.defaults.global.animationSteps / 60.0
    val delaySec     = chartTimeSec * 1.4 // some buffer
    val delayMicro   = delaySec * 1000000.0
    delayMicro.toInt.micros
  }
}

