import sbt._
import Keys._
import com.jsuereth.sbtpgp.PgpKeys
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport._
import Dependencies._
import Lib._

object ScalaJsBenchmark {

  private val ghProject = "scalajs-benchmark"

  def scalacCommonFlags = Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
  )

  def scalac2Flags = Seq(
    "-opt:l:inline",
    "-opt-inline-from:japgolly.scalajs.benchmark.**",
    "-Yno-generic-signatures",                       // Suppress generation of generic signatures for Java.
    "-Ypatmat-exhaust-depth", "off",
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

  def scalac3Flags = Seq(
    "-source", "3.0-migration",
  )

  val commonSettings: PE =
    _.settings(
      organization                  := "com.github.japgolly.scalajs-benchmark",
      homepage                      := Some(url("https://github.com/japgolly/" + ghProject)),
      licenses                      += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalaVersion                  := Ver.scala2,
      crossScalaVersions            := Seq(Ver.scala2, Ver.scala3),
      scalacOptions                ++= scalacCommonFlags,
      scalacOptions                ++= byScalaVersion {
                                         case (2, 13, 1, _) => scalac2Flags.filterNot(_.startsWith("-W"))
                                         case (2, 13, _, _) => scalac2Flags
                                         case (3,  _, _, _) => scalac3Flags
                                         case _             => Nil
                                       }.value,
      incOptions                    := incOptions.value.withLogRecompileOnMacro(false),
      updateOptions                 := updateOptions.value.withCachedResolution(true),
      releasePublishArtifactsAction := PgpKeys.publishSigned.value,
      releaseTagComment             := s"v${(ThisBuild / version).value}",
      releaseVcsSign                := true,
    )

  def utestSettings: PE =
    _.settings(
      jsEnv               := new JSDOMNodeJSEnv,
      libraryDependencies += Dep.utest.value % Test,
      libraryDependencies += Dep.microlibsTestUtil.value % Test,
      testFrameworks      := new TestFramework("utest.runner.Framework") :: Nil)

  lazy val genBoilerplate = TaskKey[Unit]("genBoilerplate")

  lazy val root =
    Project("root", file("."))
      .configure(commonSettings, preventPublication)
      .aggregate(benchmark, demo)

  // ===================================================================================================================

  lazy val benchmark =
    Project("benchmark", file("benchmark"))
      .enablePlugins(ScalaJSPlugin, JSDependenciesPlugin)
      .configure(commonSettings, publicationSettings(ghProject), utestSettings)
      .settings(
        libraryDependencies ++= Seq(
          Dep.cats               .value,
          Dep.circe              .value,
          Dep.circeGeneric       .value,
          Dep.circeParser        .value % Test,
          Dep.microlibsStdlibExt .value,
          Dep.microlibsUtils     .value,
          Dep.monocle            .value,
          Dep.monocleMacro       .value,
          Dep.scalaCss           .value,
          Dep.scalaCssReact      .value,
          Dep.scalaJsReactCore   .value,
          Dep.scalaJsReactExtra  .value,
          Dep.scalaJsReactMonocle.value,
          Dep.sourceCode         .value,
        ),

        Compile / unmanagedSourceDirectories ++= byScalaVersion {
          case (2, 13, 1, _) => "main/scala-2.13.1" :: Nil
          case _             => Nil
        }.value.map(sourceDirectory.value / _),

        jsDependencies ++= Seq(
          Dep.chartJs,
          Dep.fileSaver,
          Dep.jstat,
          Dep.jsZip,
          Dep.react,
          Dep.reactDom,
        ),

        genBoilerplate := GenBoilerplate(sourceDirectory.value / "main" / "scala")
      )

  // ===================================================================================================================

  object Demo {
    def librariesFileTask = Def.task {
      val file = (Compile / sourceManaged).value / "demo" / "SbtLibraries.scala"
      val content = s"""
           |package demo
           |
           |// This is auto-generated by sbt.
           |trait SbtLibraries {
           |  final val Cats         = Library("Cats"        , "${Ver.cats}")
           |  final val CatsEffect   = Library("Cats Effect" , "${Ver.catsEffect}")
           |  final val Monocle      = Library("Monocle"     , "${Ver.monocle}")
           |  final val Scala        = Library("Scala"       , "${scalaVersion.value}")
           |  final val ScalaJsReact = Library("ScalaJsReact", "${Ver.scalaJsReact}")
           |  final val Scalaz       = Library("Scalaz"      , "${Ver.scalaz}")
           |  final val Shapeless    = Library("Shapeless"   , "${Ver.shapeless}")
           |}
         """.stripMargin
      IO.write(file, content)
      Seq(file)
    }
  }

  lazy val demo =
    Project("demo", file("demo"))
      .enablePlugins(ScalaJSPlugin, JSDependenciesPlugin)
      .configure(commonSettings, preventPublication, utestSettings)
      .dependsOn(benchmark)
      .settings(
        libraryDependencies ++= Seq(
          Dep.cats        .value,
          Dep.catsEffect  .value,
          Dep.catsFree    .value,
          Dep.scalaz      .value,
          Dep.scalazEffect.value,
        ),
        libraryDependencies ++= Seq(
          Dep.shapeless.value,
        ).filter(_ => scalaVersion.value.startsWith("2")),
        Compile / sourceGenerators += Demo.librariesFileTask.taskValue,
        Compile / unmanagedSourceDirectories ++= addDirsFor213_+(Compile).value,
        scalaJSLinkerConfig ~= { _.withSourceMap(true) },
        packageJSDependencies / skip := false,
      )
}
