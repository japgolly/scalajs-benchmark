import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object Lib {
  type PE = Project => Project

  private val verRegex = """^(\d+)\.(\d+)\.(\d+)-?(.+)?$""".r

  def byScalaVersion[A](f: PartialFunction[(Long, Long, Long, Option[String]), Seq[A]]): Def.Initialize[Seq[A]] =
    Def.setting(
      scalaVersion.value match {
        case verRegex(a, b, c, d) => f.lift((a.toInt, b.toInt, c.toInt, Option(d).filter(_.nonEmpty))).getOrElse(Nil)
        case _                    => Nil
      }
    )

  def addCommandAliases(m: (String, String)*): PE = {
    val s = m.map(p => addCommandAlias(p._1, p._2)).reduce(_ ++ _)
    _.settings(s: _*)
  }

  def publicationSettings(ghProject: String): PE =
    sourceMapsToGithub(ghProject).andThen(
    _.settings(
      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value)
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      },
      pomExtra :=
        <scm>
          <connection>scm:git:github.com/japgolly/{ghProject}</connection>
          <developerConnection>scm:git:git@github.com:japgolly/{ghProject}.git</developerConnection>
          <url>github.com:japgolly/{ghProject}.git</url>
        </scm>
        <developers>
          <developer>
            <id>japgolly</id>
            <name>David Barri</name>
          </developer>
        </developers>))

  def sourceMapsToGithub(ghProject: String): PE =
    p => p.settings(
      scalacOptions ++= (if (isSnapshot.value) Seq.empty else Seq({
        val a = p.base.toURI.toString.replaceFirst("[^/]+/?$", "")
        val g = s"https://raw.githubusercontent.com/japgolly/$ghProject"
        s"-P:scalajs:mapSourceURI:$a->$g/v${version.value}/"
      }))
    )

  def preventPublication: PE =
    _.settings(
      publish            := {},
      publishLocal       := {},
      publishSigned      := {},
      publishLocalSigned := {},
      publishArtifact    := false,
      publishTo          := Some(Resolver.file("Unused transient repository", target.value / "fakepublish")),
      packagedArtifacts  := Map.empty)
    // .disablePlugins(plugins.IvyPlugin)
}
