import sbt._
import Keys._
import com.typesafe.sbt.pgp.PgpKeys
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport._
import Lib._

object ScalaJsBenchmark {

  private val ghProject = "scalajs-benchmark"

  object Ver {
    val BetterMonadicFor = "0.3.1"
    val ChartJs          = "1.0.2"
    val Circe            = "0.13.0"
    val MacroParadise    = "2.1.1"
    val Microlibs        = "2.3"
    val Monocle          = "1.6.3"
    val React            = "16.13.1"
    val Scala212         = "2.12.11"
    val Scala213         = "2.13.3"
    val ScalaCollCompat  = "2.1.6"
    val ScalaCss         = "0.6.1"
    val ScalaJsReact     = "1.7.0"
    val Scalaz           = "7.2.30"

    // Test only
    val MTest = "0.7.4"

    // Demo only
    val Cats      = "2.1.1"
    val Shapeless = "2.3.3"
  }

  def scalacFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-opt:l:inline",
    "-opt-inline-from:japgolly.scalajs.benchmark.**",
    "-Yno-generic-signatures",                       // Suppress generation of generic signatures for Java.
    "-Ypatmat-exhaust-depth", "off") ++
    (if (scalaJSVersion.startsWith("0.")) Seq("-P:scalajs:sjsDefinedByDefault") else Nil)

  def scalac213Flags = Seq(
    "-Wconf:msg=may.not.be.exhaustive:e",            // Make non-exhaustive matches errors instead of warnings
    "-Wunused:explicits",                            // Warn if an explicit parameter is unused.
    "-Wunused:implicits",                            // Warn if an implicit parameter is unused.
    "-Wunused:imports",                              // Warn if an import selector is not referenced.
    "-Wunused:locals",                               // Warn if a local definition is unused.
    "-Wunused:patvars",                              // Warn if a variable bound in a pattern is unused.
    "-Wunused:privates",                             // Warn if a private member is unused.
    "-Xlint:adapted-args",                           // An argument list was modified to match the receiver.
    "-Xlint:constant",                               // Evaluation of a constant arithmetic expression resulted in an error.
    "-Xlint:delayedinit-select",                     // Selecting member of DelayedInit.
    "-Xlint:deprecation",                            // Enable -deprecation and also check @deprecated annotations.
    "-Xlint:eta-zero",                               // Usage `f` of parameterless `def f()` resulted in eta-expansion, not empty application `f()`.
    "-Xlint:implicit-not-found",                     // Check @implicitNotFound and @implicitAmbiguous messages.
    "-Xlint:inaccessible",                           // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                              // A type argument was inferred as Any.
    "-Xlint:missing-interpolator",                   // A string literal appears to be missing an interpolator id.
    "-Xlint:nonlocal-return",                        // A return statement used an exception for flow control.
    "-Xlint:nullary-unit",                           // `def f: Unit` looks like an accessor; add parens to look side-effecting.
    "-Xlint:option-implicit",                        // Option.apply used an implicit view.
    "-Xlint:poly-implicit-overload",                 // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",                         // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                            // In a pattern, a sequence wildcard `_*` should match all of a repeated parameter.
    "-Xlint:valpattern",                             // Enable pattern checks in val definitions.
    "-Xmixin-force-forwarders:false",                // Only generate mixin forwarders required for program correctness.
    "-Yjar-compression-level", "9",                  // compression level to use when writing jar files
    "-Ymacro-annotations"                            // Enable support for macro annotations, formerly in macro paradise.
  )

  val commonSettings: PE =
    _.settings(
      organization                  := "com.github.japgolly.scalajs-benchmark",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.Scala213,
      crossScalaVersions            := Seq(Ver.Scala212, Ver.Scala213),
      scalacOptions                ++= scalacFlags,
      scalacOptions                ++= byScalaVersion {
                                         case (2, 13, 1, _) => scalac213Flags.filterNot(_.startsWith("-W"))
                                         case (2, 13, _, _) => scalac213Flags
                                         case _             => Nil
                                       }.value,
      shellPrompt in ThisBuild      := ((s: State) => Project.extract(s).currentRef.project + "> "),
      incOptions                    := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(version in ThisBuild).value}",
      releaseVcsSign                := true,
      addCompilerPlugin(compilerPlugin("com.olegpy" %% "better-monadic-for" % Ver.BetterMonadicFor)))

  def definesMacros: Project => Project =
    _.settings(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies ++= Seq(
        // "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        // "org.scala-lang" % "scala-library" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"))

  def addMacroParadisePlugin = Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, v)) if v <= 12 =>
        Seq(compilerPlugin("org.scalamacros" % "paradise" % Ver.MacroParadise cross CrossVersion.patch))
      case _ =>
        // if scala 2.13.0-M4 or later, macro annotations merged into scala-reflect
        // https://github.com/scala/scala/pull/6606
        Nil
    }
  }

  def utestSettings: PE =
    _.settings(
      jsEnv               := new JSDOMNodeJSEnv,
      libraryDependencies += "com.lihaoyi" %%% "utest" % Ver.MTest % Test,
      libraryDependencies += "com.github.japgolly.microlibs" %%% "test-util" % Ver.Microlibs % Test,
      testFrameworks      := new TestFramework("utest.runner.Framework") :: Nil)

  lazy val genBoilerplate = TaskKey[Unit]("genBoilerplate")

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings, preventPublication)
      .aggregate(benchmark, demo)

  // ===================================================================================================================

  lazy val benchmark =
    Project("benchmark", file("benchmark"))
      .enablePlugins(ScalaJSPlugin)
      .enablePlugins(JSDependenciesPlugin)
      .configure(commonSettings, definesMacros, publicationSettings(ghProject), utestSettings)
      .settings(
        libraryDependencies ++= addMacroParadisePlugin.value,
        libraryDependencies ++= Seq(
          "org.scala-lang.modules"            %%% "scala-collection-compat" % Ver.ScalaCollCompat,
          "com.github.japgolly.microlibs"     %%% "stdlib-ext"              % Ver.Microlibs,
          "com.github.japgolly.microlibs"     %%% "utils"                   % Ver.Microlibs,
          "com.github.japgolly.scalajs-react" %%% "core"                    % Ver.ScalaJsReact,
          "com.github.japgolly.scalajs-react" %%% "extra"                   % Ver.ScalaJsReact,
          "com.github.japgolly.scalajs-react" %%% "ext-monocle-scalaz"      % Ver.ScalaJsReact,
          "com.github.japgolly.scalacss"      %%% "core"                    % Ver.ScalaCss,
          "com.github.japgolly.scalacss"      %%% "ext-react"               % Ver.ScalaCss,
          "com.github.julien-truffaut"        %%% "monocle-core"            % Ver.Monocle,
          "com.github.julien-truffaut"        %%% "monocle-macro"           % Ver.Monocle,
          "io.circe"                          %%% "circe-core"              % Ver.Circe,
          "io.circe"                          %%% "circe-generic"           % Ver.Circe,
          "io.circe"                          %%% "circe-parser"            % Ver.Circe % Test,
          "org.scalaz"                        %%% "scalaz-core"             % Ver.Scalaz),

        Compile / unmanagedSourceDirectories ++= byScalaVersion {
          case (2, 13, 1, _) => "main/scala-2.13.1" :: Nil
          case _             => Nil
        }.value.map(sourceDirectory.value / _),

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

          "org.webjars.npm" % "jstat" % "1.9.3"
            /        "dist/jstat.js"
            minified "dist/jstat.min.js",

          "org.webjars.npm" % "file-saver" % "2.0.2"
            /        "dist/FileSaver.js"
            minified "dist/FileSaver.min.js",

          "org.webjars" % "chartjs" % Ver.ChartJs
            /        "Chart.js"
            minified "Chart.min.js"),

        genBoilerplate := GenBoilerplate(sourceDirectory.value / "main" / "scala")
      )

  // ===================================================================================================================

  object Demo {
    def librariesFileTask = Def.task {
      val file = (sourceManaged in Compile).value / "demo" / "SbtLibraries.scala"
      val content = s"""
           |package demo
           |
           |trait SbtLibraries {
           |  final val Monocle   = Library("Monocle"  , "${Ver.Monocle}")
           |  final val Scala     = Library("Scala"    , "${scalaVersion.value}")
           |  final val Cats      = Library("Cats"     , "${Ver.Cats}")
           |  final val Scalaz    = Library("Scalaz"   , "${Ver.Scalaz}")
           |  final val Shapeless = Library("Shapeless", "${Ver.Shapeless}")
           |}
         """.stripMargin
      IO.write(file, content)
      Seq(file)
    }
  }

  lazy val demo =
    Project("demo", file("demo"))
      .enablePlugins(ScalaJSPlugin)
      .enablePlugins(JSDependenciesPlugin)
      .configure(commonSettings, preventPublication, utestSettings)
      .dependsOn(benchmark)
      .settings(
        libraryDependencies ++= addMacroParadisePlugin.value,
        libraryDependencies ++= Seq(
          "org.scalaz"    %%% "scalaz-core"   % Ver.Scalaz,
          "org.scalaz"    %%% "scalaz-effect" % Ver.Scalaz,
          "org.typelevel" %%% "cats-core"     % Ver.Cats,
          "org.typelevel" %%% "cats-free"     % Ver.Cats,
          "com.chuusai"   %%% "shapeless"     % Ver.Shapeless),
        sourceGenerators in Compile += Demo.librariesFileTask.taskValue,
        scalaJSLinkerConfig ~= { _.withSourceMap(true) },
        skip in packageJSDependencies := false)
}
