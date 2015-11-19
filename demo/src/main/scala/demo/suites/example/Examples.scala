package demo.suites.example

import monocle.Iso
import scala.collection.mutable
import japgolly.scalajs.benchmark._
import Benchmark.SetupFn
import gui._
import Param._

object Examples {

  /**
    * The simplest of benchmarks takes no params and is just a collection of `() => Any`s.
    */
  val noParams = GuiSuite(
    Suite("No Params")(

      Benchmark("immutableSet") {
        var s = Set.empty[Int]
        for (i <- 1 to 100) s += i
        s
      },

      Benchmark("mutableBitSetAdd") {
        val s = mutable.BitSet.empty
        for (i <- 1 to 100) s.add(i)
        s
      }
    ))

  // ===================================================================================================================

  /**
    * These benchmarks accept a "size" parameter of type `Int`. The size is used to generate a `List[Int]` which is
    * provided to each benchmark.
    */
  val oneParam = {

    /**
      * For each size, prepare a `List[Int]` to be passed to the benchmark functions.
      */
    val bm = SetupFn.map[Int, List[Int]](size =>
      (-size to -1).toSet.iterator.map(-(_: Int)).toList)

    val suite = Suite("One Param")(

      bm("immutableSet") { is =>
        var s = Set.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      },

      bm("mutableBitSetAdd") { is =>
        val s = mutable.BitSet.empty
        for (i <- is) if (!s.add(i)) ???
        s
      }
    )

    val param = Param(Render.int, Editor.text, Parser.intsAsText)("Size", 10, 100)

    GuiSuite(suite, param)
  }

  // ===================================================================================================================

  /**
    * A composite param used to configure benchmarks in [[twoParams]].
    */
  case class Multi(size: Int, reverse: Boolean) {
    override def toString = s"$size | $reverse"
  }

  val twoParams = {

    /**
      * For each [[Multi]] param, prepare a `List[Int]` to be passed to the benchmark functions.
      */
    val bm = SetupFn.map[Multi, List[Int]] { p =>
      val x = (-p.size to -1).toSet.iterator.map(-(_: Int)).toList
      if (p.reverse) x.reverse else x
    }

    val suite = Suite("Two Params")(

      bm("immutableSet") { is =>
        var s = Set.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      },

      bm("mutableBitSetAdd") { is =>
        val s = mutable.BitSet.empty
        for (i <- is) if (!s.add(i)) ???
        s
      }
    )

    val param1 = Param(Render.int, Editor.text, Parser.intsAsText)("Size", 5, 10)
    val param2 = Param(Render.bool, Editor.text, Parser.boolsAsText)("Reverse", true, false)

    /** This specifies how to go back and forth between a [[Multi]] and two params. */
    val iso = Iso((m: Multi) => Multi.unapply(m).get)((Multi.apply _).tupled)

    val params = Params.two(iso, param1, param2)
    GuiSuite(suite, params)
  }

  // ===================================================================================================================

  /**
    * Here we group the suites into a folder that can be passed to the menu GUI ([[MenuComp]]) so that a user can choose
    * from a collection of available benchmarks.
    */
  lazy val all = MenuComp.folder("Benchmark Demos")(noParams, oneParam, twoParams)
}