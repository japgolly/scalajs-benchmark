package japgolly.scalajs.benchmark.engine

import scala.scalajs.LinkingInfo
import scala.scalajs.runtime.linkingInfo

object ScalaJsInfo {

  val version: String = {
    var ver = (linkingInfo.linkerVersion: Any) match {
      case o: Option[_] => o.fold("Unknown")("" + _)
      case s: String    => s
      case _            => "Unknown"
    }
    if (LinkingInfo.developmentMode)
      ver += "-fastOptJS"
    ver
  }

}
