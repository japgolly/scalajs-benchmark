import sbt._
import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._

object Dependencies {

  object Ver {
    val cats         = "2.7.0"
    val chartJs      = "1.0.2"
    val circe        = "0.14.1"
    val fileSaver    = "2.0.5"
    val jstat        = "1.9.3"
    val jsZip        = "3.7.1"
    val microlibs    = "4.0.0"
    val monocle      = "3.1.0"
    val react        = "17.0.2"
    val scala2       = "2.13.6"
    val scala3       = "3.0.1"
    val scalaCss     = "1.0.0"
    val scalaJsReact = "2.0.0"
    val sourceCode   = "0.2.7"

    // Test only
    val utest        = "0.7.11"

    // Demo only
    val catsEffect      = "3.3.0"
    val scalaJsJavaTime = "2.3.0"
    val scalaz          = "7.3.5"
    val shapeless       = "2.3.7"
    val zio             = "1.0.12"
  }

  object Dep {

    val cats                = Def.setting("org.typelevel"                     %%% "cats-core"               % Ver.cats)
    val catsEffect          = Def.setting("org.typelevel"                     %%% "cats-effect"             % Ver.catsEffect)
    val catsFree            = Def.setting("org.typelevel"                     %%% "cats-free"               % Ver.cats)
    val circe               = Def.setting("io.circe"                          %%% "circe-core"              % Ver.circe)
    val circeGeneric        = Def.setting("io.circe"                          %%% "circe-generic"           % Ver.circe)
    val circeParser         = Def.setting("io.circe"                          %%% "circe-parser"            % Ver.circe)
    val microlibsStdlibExt  = Def.setting("com.github.japgolly.microlibs"     %%% "stdlib-ext"              % Ver.microlibs)
    val microlibsTestUtil   = Def.setting("com.github.japgolly.microlibs"     %%% "test-util"               % Ver.microlibs)
    val microlibsUtils      = Def.setting("com.github.japgolly.microlibs"     %%% "utils"                   % Ver.microlibs)
    val monocle             = Def.setting("dev.optics"                        %%% "monocle-core"            % Ver.monocle)
    val monocleMacro        = Def.setting("dev.optics"                        %%% "monocle-macro"           % Ver.monocle)
    val scalaCss            = Def.setting("com.github.japgolly.scalacss"      %%% "core"                    % Ver.scalaCss)
    val scalaCssReact       = Def.setting("com.github.japgolly.scalacss"      %%% "ext-react"               % Ver.scalaCss)
    val scalaJsJavaTime     = Def.setting("io.github.cquiroz"                 %%% "scala-java-time"         % Ver.scalaJsJavaTime)
    val scalaJsReactCore    = Def.setting("com.github.japgolly.scalajs-react" %%% "core"                    % Ver.scalaJsReact)
    val scalaJsReactExtra   = Def.setting("com.github.japgolly.scalajs-react" %%% "extra"                   % Ver.scalaJsReact)
    val scalaJsReactMonocle = Def.setting("com.github.japgolly.scalajs-react" %%% "extra-ext-monocle3"      % Ver.scalaJsReact)
    val scalaz              = Def.setting("org.scalaz"                        %%% "scalaz-core"             % Ver.scalaz cross CrossVersion.for3Use2_13)
    val scalazEffect        = Def.setting("org.scalaz"                        %%% "scalaz-effect"           % Ver.scalaz cross CrossVersion.for3Use2_13)
    val shapeless           = Def.setting("com.chuusai"                       %%% "shapeless"               % Ver.shapeless)
    val sourceCode          = Def.setting("com.lihaoyi"                       %%% "sourcecode"              % Ver.sourceCode)
    val utest               = Def.setting("com.lihaoyi"                       %%% "utest"                   % Ver.utest)
    val zio                 = Def.setting("dev.zio"                           %%% "zio"                     % Ver.zio)

    val react = (
      "org.webjars.npm" % "react" % Ver.react
      /        "umd/react.development.js"
      minified "umd/react.production.min.js"
      commonJSName "React")

    val reactDom = (
      "org.webjars.npm" % "react-dom" % Ver.react
      /         "umd/react-dom.development.js"
      minified  "umd/react-dom.production.min.js"
      dependsOn "umd/react.development.js"
      commonJSName "ReactDOM")

    val jstat = (
      "org.webjars.npm" % "jstat" % Ver.jstat
      /        "dist/jstat.js"
      minified "dist/jstat.min.js")

    val fileSaver = (
      "org.webjars.npm" % "file-saver" % Ver.fileSaver
      /        "dist/FileSaver.js"
      minified "dist/FileSaver.min.js")

    val jsZip = (
      "org.webjars.npm" % "jszip" % Ver.jsZip
      /        "dist/jszip.js"
      minified "dist/jszip.min.js")

    val chartJs = (
      "org.webjars" % "chartjs" % Ver.chartJs
      /        "Chart.js"
      minified "Chart.min.js")
  }
}
