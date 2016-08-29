package demo

import japgolly.scalajs.benchmark.{Versions => CoreVersions}

case class Library(name: String, version: String) {
  val fullName = s"$name v$version"
}

object Libraries {
  val Monocle   = Library("Monocle"  , CoreVersions.Monocle)
  val Scala     = Library("Scala"    , CoreVersions.Scala211)
  val Cats      = Library("Cats"     , Versions.Cats)
  val Scalaz    = Library("Scalaz"   , Versions.Scalaz)
  val Shapeless = Library("Shapeless", Versions.Shapeless)
}
