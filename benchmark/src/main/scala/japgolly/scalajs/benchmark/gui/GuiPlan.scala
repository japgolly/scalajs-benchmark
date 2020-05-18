package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.Plan
import japgolly.scalajs.benchmark.engine.EngineOptions
import scala.concurrent.duration.FiniteDuration

trait GuiPlan {
  type Param
  val guiSuite: GuiSuite[Param]
  val params  : Vector[Param]

  final lazy val plan: Plan[Param] =
    Plan(guiSuite.suite, params)

  final lazy val totalBMs: Int =
    guiSuite.suite.bms.length * params.length

  def eta(o: EngineOptions): FiniteDuration =
    o.estimatedTimePerBM * totalBMs

  def etaMs(o: EngineOptions): Double =
    o.estimatedMsPerBM * totalBMs
}

object GuiPlan {
  type WithParam[P] = GuiPlan { type Param = P }

  def apply[P](guiSuite: GuiSuite[P])(params: Vector[P]): WithParam[P] = {
    val _guiSuite = guiSuite
    val _params   = params
    new GuiPlan {
      override type Param   = P
      override val guiSuite = _guiSuite
      override val params   = _params
    }
  }
}