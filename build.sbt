name                     := "scalajs-benchmark"
ThisBuild / homepage     := Some(url("https://github.com/japgolly/scalajs-benchmark"))
ThisBuild / licenses     += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0"))
ThisBuild / organization := "com.github.japgolly.scalajs-benchmark"
ThisBuild / shellPrompt  := ((s: State) => Project.extract(s).currentRef.project + "> ")
ThisBuild / startYear    := Some(2015)

val root      = ScalaJsBenchmark.root
val benchmark = ScalaJsBenchmark.benchmark
val demo      = ScalaJsBenchmark.demo
