name                    := "scalajs-benchmark"
sonatypeProfileName     := "com.github.japgolly"
ThisBuild / shellPrompt := ((s: State) => Project.extract(s).currentRef.project + "> ")

val root      = ScalaJsBenchmark.root
val benchmark = ScalaJsBenchmark.benchmark
val demo      = ScalaJsBenchmark.demo
