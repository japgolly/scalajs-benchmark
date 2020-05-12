package demo

import scala.scalajs.LinkingInfo
import scala.scalajs.runtime.linkingInfo

final case class Library(name: String, version: String) {
  val fullName = s"$name v$version"
}

object Libraries extends SbtLibraries {

  val ScalaJs = {
    var ver = (linkingInfo.linkerVersion: Any) match {
      case o: Option[_] => o.fold("Unknown")("" + _)
      case s: String    => s
      case _            => "Unknown"
    }
    if (LinkingInfo.developmentMode)
      ver += "-fastOptJS"
    Library("Scala.JS", ver)
  }
}

