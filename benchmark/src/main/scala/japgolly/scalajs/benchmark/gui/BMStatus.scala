package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.Result

sealed trait BMStatus

case object BMPending extends BMStatus

case object BMPreparing extends BMStatus

case object BMRunning extends BMStatus

final case class BMDone(result: Result) extends BMStatus
