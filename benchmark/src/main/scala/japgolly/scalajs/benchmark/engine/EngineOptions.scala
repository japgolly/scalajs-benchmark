package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark.vendor.chartjs.Chart
import japgolly.scalajs.react.Reusability
import java.util.concurrent.TimeUnit
import scala.annotation.nowarn
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

  val actualWarmupIterationTime: FiniteDuration =
    warmupIterationTime.getOrElse(iterationTime)

  val estimatedTimePerBM: FiniteDuration =
    EngineOptions.estimatedOverheadPerBm +
      warmupIterations * actualWarmupIterationTime +
      iterationTime * iterations

  val estimatedMsPerBM: Double =
    TimeUtil.toMs(estimatedTimePerBM)
}

object EngineOptions {

  lazy val default: EngineOptions =
    apply(
      clock               = Clock.Default,
      initialDelay        = 4.millis,
      delay               = defaultDelay,
      warmupIterationTime = None,
      warmupIterations    = 1,
      iterations          = 10,
      iterationTime       = 1.seconds,
    )

  // Ensure benchmarks don't start before chart animation finishes
  private def defaultDelay: () => FiniteDuration = () => {
    val chartTimeSec = Chart.defaults.global.animationSteps / 60.0
    val delaySec     = chartTimeSec * 1.4 // some buffer
    val delayMicro   = delaySec * 1000000.0
    delayMicro.toInt.micros
  }

  private val estimatedOverheadPerBm =
    FiniteDuration(2000, TimeUnit.MILLISECONDS)

  implicit val reusability: Reusability[EngineOptions] = {
    @nowarn("cat=unused") implicit val x1 = Reusability.byRef[Clock]
    @nowarn("cat=unused") implicit val x2 = Reusability.byRef[() => FiniteDuration]
    @nowarn("cat=unused") implicit val x3 = Reusability.byRef[FiniteDuration] // TODO https://github.com/japgolly/scalajs-react/issues/719
    Reusability.byRef || Reusability.derive
  }
}
