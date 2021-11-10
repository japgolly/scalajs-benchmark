scalajs-benchmark
=================

[![Build Status](https://travis-ci.org/japgolly/scalajs-benchmark.svg?branch=master)](https://travis-ci.org/japgolly/scalajs-benchmark)

Benchmarks: write in Scala or JS, run in your browser.

See an online demo here: https://japgolly.github.io/scalajs-benchmark/.
<br>The tiny source code is here: [gh-pages/demo](https://github.com/japgolly/scalajs-benchmark/blob/gh-pages/demo/src/main/scala/demo/Main.scala).

[Changelogs](doc/changelog) — [Latest: 0.10.0](doc/changelog/0.10.0.md).

## How do I use it?

1. Include the Scala.js and JsDependencies plugins in your sbt `project/plugins.sbt` file:
  ```scala
  addSbtPlugin("org.scala-js" %% "sbt-scalajs"  % "1.7.1")
  addSbtPlugin("org.scala-js" % "sbt-jsdependencies" % "1.0.2")
  ```

2. Enable the plugins in your sbt project.
  ```scala
  enablePlugins(ScalaJSPlugin, JSDependenciesPlugin)
  ```

3. Include `scalajs-benchmark` as a dependency in your Scala.JS project, along with chartjs.
  ```scala
  libraryDependencies += "com.github.japgolly.scalajs-benchmark" %%% "benchmark" % "0.10.0"
  
  jsDependencies += "org.webjars" % "chartjs" % "1.0.2" / "Chart.js" minified "Chart.min.js"
  ```

4. Write benchmarks your benchmarks.
  ```scala
  import japgolly.scalajs.benchmark._
  import japgolly.scalajs.benchmark.gui._

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

5. Add a tiny loader HTML [like this](demo/scala213-full.html) file called `index.html` to the root directory of your project. In this example our project is called 'demo'. `demo-fastopt.js` is the result of our compiled benchmark suite, while `demo-jsdeps.js` is produced by the `JsDependencies` plugin.:
  ```html
  <!DOCTYPE html>
  <html>
  <head lang="en">
      <title>scalajs-benchmarks</title>
  </head>
  <body>
      <div id="body">Loading...</div>
      <script type="text/javascript" src="target/scala-3.0.1/demo-jsdeps.js"></script>
      <script type="text/javascript" src="target/scala-3.0.1/demo-fastopt.js"></script>
      <script type="text/javascript">main();</script>
  </body>
  </html>
  ```

6. Create a main app and point `scalajs-benchmark` at your suite of benchmarks.

  ```scala
  import org.scalajs.dom.document
  import japgolly.scalajs.benchmark.gui.BenchmarkGUI

  object Main {

    def main(): Unit = {
      val body = document getElementById "body"
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

7. Compile and run in browser. One way to do this is the use the `fastOptJS` sbt command, and then run a local [http server](https://www.npmjs.com/package/http-server) from the root directory of your project.


## Support
If you like what I do
—my OSS libraries, my contributions to other OSS libs, [my programming blog](https://japgolly.blogspot.com)—
and you'd like to support me, more content, more lib maintenance, [please become a patron](https://www.patreon.com/japgolly)!
I do all my OSS work unpaid so showing your support will make a big difference.

---

*Note: This is not affiliated with the official [Scala.JS](http://www.scala-js.org/) project; it's just named to be informative rather than interesting.*
