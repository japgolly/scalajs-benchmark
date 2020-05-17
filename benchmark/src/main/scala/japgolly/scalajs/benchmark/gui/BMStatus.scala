package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.Result

sealed trait BMStatus

object BMStatus {

  case object Pending extends BMStatus

  case object Preparing extends BMStatus

  case object Running extends BMStatus

  final case class Done(result: Result) extends BMStatus

}
