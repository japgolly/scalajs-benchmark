import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.cross.CrossProject
import ScalaJSPlugin._
import ScalaJSPlugin.autoImport._

object Lib {
  type PE = Project => Project

  def addCommandAliases(m: (String, String)*): PE = {
    val s = m.map(p => addCommandAlias(p._1, p._2)).reduce(_ ++ _)
    _.settings(s: _*)
  }

  val clearScreenTask = TaskKey[Unit]("clear", "Clears the screen.")

  def clearScreen(): Unit =
    println("\033[2J\033[;H")

  implicit class CrossProjectExt(val cp: CrossProject) extends AnyVal {
    def bothConfigure(f: PE): CrossProject =
      cp.jvmConfigure(f).jsConfigure(f)
  }
}
