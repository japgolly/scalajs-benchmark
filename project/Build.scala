import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin.autoImport._
import Lib._

object ScalaJsBenchmark extends Build {

  private val ghProject = "scalajs-benchmark"

  object Ver {
    final val ChartJs       = "1.0.2"
    final val MacroParadise = "2.1.0"
    final val Monocle       = "1.2.0-2"
    final val React         = "15.0.1"
    final val Scala211      = "2.11.8"
    final val ScalaCss      = "0.4.1"
    final val ScalaJsReact  = "0.11.0"
  }

  def scalacFlags = Seq(
    "-deprecation", "-unchecked", "-feature",
    "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

  val commonSettings: PE =
    _.settings(
      organization             := "com.github.japgolly.scalajs-benchmark",
      version                  := "0.2.3",
      homepage                 := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                 += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion             := Ver.Scala211,
      scalacOptions           ++= scalacFlags,
      shellPrompt in ThisBuild := ((s: State) => Project.extract(s).currentRef.project + "> "),
      triggeredMessage         := Watched.clearWhenTriggered,
      incOptions               := incOptions.value.withNameHashing(true),
      updateOptions            := updateOptions.value.withCachedResolution(true))
    .configure(
      addCommandAliases(
        "/"   -> "project root",
        "C"   -> "root/clean",
        "T"   -> ";root/clean;root/test",
        "c"   -> "compile",
        "tc"  -> "test:compile",
        "t"   -> "test",
        "cc"  -> ";clean;compile",
        "ctc" -> ";clean;test:compile",
        "ct"  -> ";clean;test"))

  def definesMacros: Project => Project =
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= Seq(
        // "org.scala-lang" % "scala-reflect" % Ver.Scala211,
        // "org.scala-lang" % "scala-library" % Ver.Scala211,
        "org.scala-lang" % "scala-compiler" % Ver.Scala211 % "provided"))

  def macroParadisePlugin =
    compilerPlugin("org.scalamacros" % "paradise" % Ver.MacroParadise cross CrossVersion.full)

  override def rootProject = Some(root)

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings, preventPublication)
      .aggregate(benchmark, demo)

  lazy val benchmark =
    Project("benchmark", file("benchmark"))
      .enablePlugins(ScalaJSPlugin)
      .configure(commonSettings, definesMacros, publicationSettings(ghProject))
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
          "org.webjars.bower" % "react" % Ver.React
            /        "react-with-addons.js"
            minified "react-with-addons.min.js"
            commonJSName "React",

          "org.webjars.bower" % "react" % Ver.React
            /         "react-dom.js"
            minified  "react-dom.min.js"
            dependsOn "react-with-addons.js"
            commonJSName "ReactDOM",

          "org.webjars" % "chartjs" % Ver.ChartJs
            /        "Chart.js"
            minified "Chart.min.js"),

        addCompilerPlugin(macroParadisePlugin),
        test := ())

  object Demo {
    val outputJS     = "output.js"
    val catsVer      = "0.4.1"
    val scalazVer    = "7.2.2"
    val shapelessVer = "2.3.0"
  }
  lazy val demo =
    Project("demo", file("demo"))
      .enablePlugins(ScalaJSPlugin)
      .configure(commonSettings, preventPublication)
      .dependsOn(benchmark)
      .settings(
        addCompilerPlugin(macroParadisePlugin),
        libraryDependencies ++= Seq(
          "org.scalaz"    %%% "scalaz-core"       % Demo.scalazVer,
          "org.scalaz"    %%% "scalaz-effect"     % Demo.scalazVer,
          "org.typelevel" %%% "cats"              % Demo.catsVer,
          "com.chuusai"   %%% "shapeless"         % Demo.shapelessVer),
        skip in packageJSDependencies := false,
        artifactPath in (Compile, fastOptJS) := ((target in Compile).value / Demo.outputJS),
        artifactPath in (Compile, fullOptJS) := ((target in Compile).value / Demo.outputJS),
        test := ())
}
