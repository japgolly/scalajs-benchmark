import sbt._
import Keys._
import pl.project13.scala.sbt.JmhPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin
//import ScalaJSPlugin._
import ScalaJSPlugin.autoImport._
import Lib._

object ScalaJsBenchmark extends Build {

  object Ver {
    final val Scala211      = "2.11.7"
    final val MacroParadise = "2.0.1"
    final val Monocle       = "1.1.1"
    final val ScalaCss      = "0.3.1"
    final val ScalaJsReact  = "0.10.1"
    final val React         = "0.14.2"
    final val ChartJs       = "1.0.2"
  }

  def scalacFlags = Seq(
    "-deprecation", "-unchecked", "-feature",
    "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

  val commonSettings: PE =
    _.settings(
      organization             := "com.github.japgolly.tempname",
      version                  := "0.1.0-SNAPSHOT",
      homepage                 := Some(url("https://github.com/japgolly/tempname")),
      licenses                 += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion             := Ver.Scala211,
      scalacOptions           ++= scalacFlags,
      clearScreenTask          := clearScreen(),
      shellPrompt in ThisBuild := ((s: State) => Project.extract(s).currentRef.project + "> "),
      incOptions               := incOptions.value.withNameHashing(true),
      updateOptions            := updateOptions.value.withCachedResolution(true))
    .configure(
      addCommandAliases(
        "/"    -> "project root",
        "C"    -> "root/clean",
        "cc"   -> ";clear;compile",
        "ctc"  -> ";clear;test:compile",
        "ct"   -> ";clear;test",
        "cq"   -> ";clear;testQuick",
        "ccc"  -> ";clear;clean;compile",
        "cctc" -> ";clear;clean;test:compile",
        "cct"  -> ";clear;clean;test"))

  def macroParadisePlugin =
    compilerPlugin("org.scalamacros" % "paradise" % Ver.MacroParadise cross CrossVersion.full)

  override def rootProject = Some(root)

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings)
      .aggregate(benchmark, demo)

  lazy val benchmark =
    Project("benchmark", file("benchmark"))
      .enablePlugins(ScalaJSPlugin)
      .configure(commonSettings)
      .settings(
        libraryDependencies ++= Seq(
          "com.github.japgolly.scalajs-react" %%% "core"          % Ver.ScalaJsReact,
          "com.github.japgolly.scalajs-react" %%% "extra"         % Ver.ScalaJsReact,
          "com.github.japgolly.scalajs-react" %%% "ext-monocle"   % Ver.ScalaJsReact,
          "com.github.japgolly.scalacss"      %%% "core"          % Ver.ScalaCss,
          "com.github.japgolly.scalacss"      %%% "ext-react"     % Ver.ScalaCss,
          "com.github.japgolly.fork.monocle"  %%% "monocle-core"  % Ver.Monocle,
          "com.github.japgolly.fork.monocle"  %%% "monocle-macro" % Ver.Monocle),
        jsDependencies ++= Seq(
          "org.webjars.npm" % "react"     % Ver.React   / "react-with-addons.js" commonJSName "React"    minified "react-with-addons.min.js",
          "org.webjars.npm" % "react-dom" % Ver.React   / "react-dom.js"         commonJSName "ReactDOM" minified "react-dom.min.js" dependsOn "react-with-addons.js",
          "org.webjars"     % "chartjs"   % Ver.ChartJs / "Chart.js"                                     minified "Chart.min.js"),
        addCompilerPlugin(macroParadisePlugin))

  val demoJs = "output.js"
  lazy val demo =
    Project("demo", file("demo"))
      .enablePlugins(ScalaJSPlugin)
      .configure(commonSettings)
      .dependsOn(benchmark)
      .settings(
        addCompilerPlugin(macroParadisePlugin),
        skip in packageJSDependencies := false,
        artifactPath in (Compile, fastOptJS) := ((target in Compile).value / demoJs),
        artifactPath in (Compile, fullOptJS) := ((target in Compile).value / demoJs))
}
