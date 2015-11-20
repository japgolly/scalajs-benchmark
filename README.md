scalajs-benchmark
=================

[![Build Status](https://travis-ci.org/japgolly/scalajs-react.svg?branch=master)](https://travis-ci.org/japgolly/scalajs-benchmark)

Benchmarks: write in Scala or JS, run in your browser.

See an online demo here: https://japgolly.github.io/scalajs-benchmark/.
<br>The tiny source code is here: [gh-pages/demo](https://github.com/japgolly/scalajs-benchmark/blob/gh-pages/demo/src/main/scala/demo/Main.scala).

## How do I use it?

1. Include `scalajs-benchmark` as a dependency in your Scala.JS project.
  ```scala
  libraryDependencies += "com.github.japgolly.scalajs-benchmark" %%% "benchmark" % "0.1.0"
  ```

1. You write benchmarks.
  ```scala
  import japgolly.scalajs.benchmark._, gui._
  
  object Example {
    val suite = GuiSuite(
      Suite("Example Benchmarks")(

        // Benchmark #1
        Benchmark("foreach") {
          var s = Set.empty[Int]
          (1 to 100) foreach (s += _)
          s
        },

        // Benchmark #2
        Benchmark("fold") {
          (1 to 100).foldLeft(Set.empty[Int])(_ + _)
        }
      )
    )
  }
  ```
  
  *(Hey, can you make that `1 to 100` configurable in the GUI? [You sure can.](https://github.com/japgolly/scalajs-benchmark/blob/master/demo/src/main/scala/demo/suites/example/Examples.scala))*

1. Add a tiny loader HTML [like this](demo/index.html).

1. Create a `JSApp` and point `scalajs-benchmark` at your suite of benchmarks.

  ```scala
  import japgolly.scalajs.benchmark.gui.BenchmarkGUI
  
  object Main extends scalajs.js.JSApp {
    def main() = {
      val body = org.scalajs.dom.document getElementById "body"
      
      BenchmarkGUI.renderSuite(body)(Example.suite)
    }
  }
  ```
  
  If you have a library of different benchmarks, instead of using `renderSuite`,
  you use `renderMenu` to create an app that lets the user navigate the library and
  run benchmarks.
  
  Example:

  ```scala
  BenchmarkGUI.renderMenu(body)(
    suites.example.Examples.all,
    suites.scala.all)
  ```

1. Compile; run in browser. Done.

---

*Note: This is not affiliated with the official [Scala.JS](http://www.scala-js.org/) project; it's just named to be informative rather than interesting.*
