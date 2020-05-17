package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.Result

sealed trait BMStatus {
  import BMStatus._

  final def merge(s: BMStatus): BMStatus =
    (this, s) match {
      case (_: Done, x) => x
      case (x, _: Done) => x
      case (Pending, x) => x
      case (x, Pending) => x

      // These cases shouldn't happen in practice
      case (Running, Running | Preparing) => Running
      case (Preparing, Running)           => Running
      case (Preparing, Preparing)         => Preparing
    }
}

object BMStatus {

  case object Pending extends BMStatus

  case object Preparing extends BMStatus

  case object Running extends BMStatus

  final case class Done(result: Result) extends BMStatus

}
