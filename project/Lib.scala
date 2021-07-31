import sbt._
import Keys._
import com.jsuereth.sbtpgp.PgpKeys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import xerial.sbt.Sonatype.autoImport._

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
      publishTo := sonatypePublishToBundle.value,
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

  def sourceMapsToGithub(ghProject: String): Project => Project =
    p => p.settings(
      scalacOptions ++= {
        val isDotty = scalaVersion.value startsWith "3"
        val ver     = version.value
        if (isSnapshot.value)
          Nil
        else {
          val a = p.base.toURI.toString.replaceFirst("[^/]+/?$", "")
          val g = s"https://raw.githubusercontent.com/japgolly/$ghProject"
          val flag = if (isDotty) "-scalajs-mapSourceURI" else "-P:scalajs:mapSourceURI"
          s"$flag:$a->$g/v$ver/" :: Nil
        }
      }
    )

  def preventPublication: PE =
    _.settings(publish / skip := true)
}
