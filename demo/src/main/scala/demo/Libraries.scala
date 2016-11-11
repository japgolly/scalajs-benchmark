package demo

case class Library(name: String, version: String) {
  val fullName = s"$name v$version"
}

object Libraries extends SbtLibraries {

  val ScalaJs = {
    import scalajs.runtime.linkingInfo
    var ver = linkingInfo.linkerVersion.getOrElse("Unknown")
    if (!linkingInfo.semantics.productionMode)
      ver += "-dev"
    Library("Scala.JS", ver)
  }
}

