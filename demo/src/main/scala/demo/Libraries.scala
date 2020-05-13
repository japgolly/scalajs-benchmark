package demo

import japgolly.scalajs.benchmark.engine.ScalaJsInfo

final case class Library(name: String, version: String) {
  val fullName = s"$name v$version"
}

object Libraries extends SbtLibraries {

  val ScalaJs =
    Library("Scala.JS", ScalaJsInfo.version)
}

