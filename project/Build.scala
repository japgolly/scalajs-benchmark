import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys
import org.scalajs.sbtplugin.ScalaJSPlugin, ScalaJSPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport._
import Lib._

object ScalaJsBenchmark {

  private val ghProject = "scalajs-benchmark"

  object Ver {
    val ChartJs       = "1.0.2"
    val MacroParadise = "2.1.1"
    val Monocle       = "1.5.0"
    val React         = "16.5.1"
    val Scala211      = "2.11.12"
    val Scala212      = "2.12.6"
    val ScalaCss      = "0.5.5"
    val ScalaJsReact  = "1.3.1"
  }

  def scalacFlags = Seq(
    "-deprecation", "-unchecked", "-feature",
    "-language:postfixOps", "-language:implicitConversions", "-language:higherKinds", "-language:existentials")

  val commonSettings: PE =
    _.settings(
      organization                  := "com.github.japgolly.scalajs-benchmark",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.Scala212,
      crossScalaVersions            := Seq(Ver.Scala211, Ver.Scala212),
      scalacOptions                ++= scalacFlags,
      scalacOptions                ++= byScalaVer(Seq.empty[String], Seq("-opt:l:method")).value,
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      incOptions                    := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      triggeredMessage              := Watched.clearWhenTriggered)

  def definesMacros: Project => Project =
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= Seq(
        // "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        // "org.scala-lang" % "scala-library" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"))

  def macroParadisePlugin =
    compilerPlugin("org.scalamacros" % "paradise" % Ver.MacroParadise cross CrossVersion.full)

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
          "com.github.julien-truffaut"        %%% "monocle-core"  % Ver.Monocle,
          "com.github.julien-truffaut"        %%% "monocle-macro" % Ver.Monocle),

        dependencyOverrides += "org.webjars.npm" % "js-tokens" % "3.0.2", // https://github.com/webjars/webjars/issues/1789

        jsDependencies ++= Seq(
          "org.webjars.npm" % "react" % Ver.React
            /        "umd/react.development.js"
            minified "umd/react.production.min.js"
            commonJSName "React",

          "org.webjars.npm" % "react-dom" % Ver.React
            /         "umd/react-dom.development.js"
            minified  "umd/react-dom.production.min.js"
            dependsOn "umd/react.development.js"
            commonJSName "ReactDOM",

          "org.webjars" % "chartjs" % Ver.ChartJs
            /        "Chart.js"
            minified "Chart.min.js"),

        addCompilerPlugin(macroParadisePlugin),
        test := {})

  object Demo {
    val Cats      = "1.6.1"
    val Scalaz    = "7.2.27"
    val Shapeless = "2.3.2"

    def librariesFileTask = Def.task {
      val file = (sourceManaged in Compile).value / "demo" / "SbtLibraries.scala"
      val content = s"""
           |package demo
           |
           |trait SbtLibraries {
           |  final val Monocle   = Library("Monocle"  , "${Ver.Monocle}")
           |  final val Scala     = Library("Scala"    , "${scalaVersion.value}")
           |  final val Cats      = Library("Cats"     , "$Cats")
           |  final val Scalaz    = Library("Scalaz"   , "$Scalaz")
           |  final val Shapeless = Library("Shapeless", "$Shapeless")
           |}
         """.stripMargin
      IO.write(file, content)
      Seq(file)
    }
  }
  lazy val demo =
    Project("demo", file("demo"))
      .enablePlugins(ScalaJSPlugin)
      .configure(commonSettings, preventPublication)
      .dependsOn(benchmark)
      .settings(
        addCompilerPlugin(macroParadisePlugin),
        libraryDependencies ++= Seq(
          "org.scalaz"    %%% "scalaz-core"   % Demo.Scalaz,
          "org.scalaz"    %%% "scalaz-effect" % Demo.Scalaz,
          "org.typelevel" %%% "cats-core"     % Demo.Cats,
          "org.typelevel" %%% "cats-free"     % Demo.Cats,
          "com.chuusai"   %%% "shapeless"     % Demo.Shapeless),
        sourceGenerators in Compile += Demo.librariesFileTask.taskValue,
        skip in packageJSDependencies := false,
        test := { (compile in Test).value; () })
}
