import sbt._
import sbt.Keys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin
import org.scalajs.jsdependencies.sbtplugin.JSDependenciesPlugin.autoImport._

object Dependencies {

  object Ver {
    val betterMonadicFor = "0.3.1"
    val chartJs          = "1.0.2"
    val circe            = "0.13.0"
    val fileSaver        = "2.0.2"
    val jstat            = "1.9.3"
    val jsZip            = "3.5.0"
    val macroParadise    = "2.1.1"
    val microlibs        = "2.5"
    val monocle          = "1.6.3"
    val react            = "17.0.2"
    val scala212         = "2.12.13"
    val scala213         = "2.13.6"
    val scalaCollCompat  = "2.3.2"
    val scalaCss         = "0.7.0"
    val scalaJsReact     = "1.7.7"
    val scalaz           = "7.2.30"

    // Test only
    val utest = "0.7.10"

    // Demo only
    val cats      = "2.6.1"
    val shapeless = "2.3.7"
  }

  object Dep {

    val cats               = Def.setting("org.typelevel"                     %%% "cats-core"               % Ver.cats)
    val catsFree           = Def.setting("org.typelevel"                     %%% "cats-free"               % Ver.cats)
    val circe              = Def.setting("io.circe"                          %%% "circe-core"              % Ver.circe)
    val circeGeneric       = Def.setting("io.circe"                          %%% "circe-generic"           % Ver.circe)
    val circeParser        = Def.setting("io.circe"                          %%% "circe-parser"            % Ver.circe)
    val microlibsStdlibExt = Def.setting("com.github.japgolly.microlibs"     %%% "stdlib-ext"              % Ver.microlibs)
    val microlibsTestUtil  = Def.setting("com.github.japgolly.microlibs"     %%% "test-util"               % Ver.microlibs)
    val microlibsUtils     = Def.setting("com.github.japgolly.microlibs"     %%% "utils"                   % Ver.microlibs)
    val monocle            = Def.setting("com.github.julien-truffaut"        %%% "monocle-core"            % Ver.monocle)
    val monocleMacro       = Def.setting("com.github.julien-truffaut"        %%% "monocle-macro"           % Ver.monocle)
    val scalaCollCompat    = Def.setting("org.scala-lang.modules"            %%% "scala-collection-compat" % Ver.scalaCollCompat)
    val scalaCss           = Def.setting("com.github.japgolly.scalacss"      %%% "core"                    % Ver.scalaCss)
    val scalaCssReact      = Def.setting("com.github.japgolly.scalacss"      %%% "ext-react"               % Ver.scalaCss)
    val scalaJsReactCore   = Def.setting("com.github.japgolly.scalajs-react" %%% "core"                    % Ver.scalaJsReact)
    val scalaJsReactExtra  = Def.setting("com.github.japgolly.scalajs-react" %%% "extra"                   % Ver.scalaJsReact)
    val scalaJsReactScalaz = Def.setting("com.github.japgolly.scalajs-react" %%% "ext-monocle-scalaz"      % Ver.scalaJsReact)
    val scalaz             = Def.setting("org.scalaz"                        %%% "scalaz-core"             % Ver.scalaz)
    val scalazEffect       = Def.setting("org.scalaz"                        %%% "scalaz-effect"           % Ver.scalaz)
    val shapeless          = Def.setting("com.chuusai"                       %%% "shapeless"               % Ver.shapeless)
    val utest              = Def.setting("com.lihaoyi"                       %%% "utest"                   % Ver.utest)

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

    // Compiler plugins
    val betterMonadicFor = compilerPlugin("com.olegpy"      %% "better-monadic-for" % Ver.betterMonadicFor)
    val macroParadise    = compilerPlugin("org.scalamacros"  % "paradise"           % Ver.macroParadise cross CrossVersion.patch)
  }
}
