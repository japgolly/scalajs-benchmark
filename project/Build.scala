import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin.autoImport._
import Lib._
import japgolly.scalajs.benchmark.Versions

object ScalaJsBenchmark extends Build {

  private val ghProject = "scalajs-benchmark"

  def scalacFlags = Seq(
    "-deprecation", "-unchecked", "-feature",
    "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

  val commonSettings: PE =
    _.settings(
      organization             := "com.github.japgolly.scalajs-benchmark",
      version                  := "0.2.4-SNAPSHOT",
      homepage                 := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                 += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion             := Versions.Scala211,
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
        // "org.scala-lang" % "scala-reflect" % Versions.Scala211,
        // "org.scala-lang" % "scala-library" % Versions.Scala211,
        "org.scala-lang" % "scala-compiler" % Versions.Scala211 % "provided"))

  def macroParadisePlugin =
    compilerPlugin("org.scalamacros" % "paradise" % Versions.MacroParadise cross CrossVersion.full)

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
          "com.github.japgolly.scalajs-react" %%% "core"          % Versions.ScalaJsReact,
          "com.github.japgolly.scalajs-react" %%% "extra"         % Versions.ScalaJsReact,
          "com.github.japgolly.scalajs-react" %%% "ext-monocle"   % Versions.ScalaJsReact,
          "com.github.japgolly.scalacss"      %%% "core"          % Versions.ScalaCss,
          "com.github.japgolly.scalacss"      %%% "ext-react"     % Versions.ScalaCss,
          "com.github.julien-truffaut"        %%% "monocle-core"  % Versions.Monocle,
          "com.github.julien-truffaut"        %%% "monocle-macro" % Versions.Monocle),

        jsDependencies ++= Seq(
          "org.webjars.bower" % "react" % Versions.React
            /        "react-with-addons.js"
            minified "react-with-addons.min.js"
            commonJSName "React",

          "org.webjars.bower" % "react" % Versions.React
            /         "react-dom.js"
            minified  "react-dom.min.js"
            dependsOn "react-with-addons.js"
            commonJSName "ReactDOM",

          "org.webjars" % "chartjs" % Versions.ChartJs
            /        "Chart.js"
            minified "Chart.min.js"),

        addCompilerPlugin(macroParadisePlugin),
        test := ())

  object Demo {
    val outputJS = "output.js"
    val Ver = _root_.demo.Versions
  }
  lazy val demo =
    Project("demo", file("demo"))
      .enablePlugins(ScalaJSPlugin)
      .configure(commonSettings, preventPublication)
      .dependsOn(benchmark)
      .settings(
        addCompilerPlugin(macroParadisePlugin),
        libraryDependencies ++= Seq(
          "org.scalaz"    %%% "scalaz-core"       % Demo.Ver.Scalaz,
          "org.scalaz"    %%% "scalaz-effect"     % Demo.Ver.Scalaz,
          "org.typelevel" %%% "cats"              % Demo.Ver.Cats,
          "com.chuusai"   %%% "shapeless"         % Demo.Ver.Shapeless),
        skip in packageJSDependencies := false,
        artifactPath in (Compile, fastOptJS) := ((target in Compile).value / Demo.outputJS),
        artifactPath in (Compile, fullOptJS) := ((target in Compile).value / Demo.outputJS),
        test := ())
}
