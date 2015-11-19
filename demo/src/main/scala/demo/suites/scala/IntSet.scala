package demo.suites.scala

import demo.Util._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.vdom.prefix_<^._
import scala.collection.immutable._
import scala.collection.mutable

object IntSet {

  val bm = Benchmark.setup[Int, List[Int]](size =>
    // Puts it in a non-linear, deterministic order then change to disrupt hash order
    (-size to -1).toSet.iterator.map(-(_: Int)).toList)

  val suite = Suite("Int Set")(

    bm("immutable.Set"){ is =>
      var s = Set.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("mutable.Set"){ is =>
      val s = mutable.Set.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("immutable.IntMap"){ is =>
      var s = IntMap.empty[Unit]
      for (i <- is) if (s contains i) ??? else s = s.updated(i, ())
      s
    },

//      bm("immutable.ListSet"){ is =>
//        var s = ListSet.empty[Int]
//        for (i <- is) if (s contains i) ??? else s += i
//        s
//      }),

    bm("immutable.HashSet"){ is =>
      var s = HashSet.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("mutable.HashSet"){ is =>
      val s = mutable.HashSet.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("immutable.TreeSet"){ is =>
      var s = TreeSet.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("mutable.TreeSet"){ is =>
      val s = mutable.TreeSet.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("immutable.BitSet"){ is =>
      var s = BitSet.empty
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("mutable.BitSet (contains and +=)"){ is =>
      val s = mutable.BitSet.empty
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("mutable.BitSet (add)"){ is =>
      val s = mutable.BitSet.empty
      for (i <- is) if (!s.add(i)) ???
      s
    },

    // ===============================================================================================================
    // Add then check by ref

    bm("immutable.Set (eq)"){ is =>
      var s = Set.empty[Int]
      for (i <- is) {
        val b = s; s += i; if (b eq s) ???
      }
      s
    },

//      bm("immutable.ListSet (eq)"){ is =>
//        var s = ListSet.empty[Int]
//        for (i <- is) {
//          val b = s; s += i; if (b eq s) ???
//        }
//        s
//      }),

    bm("immutable.HashSet (eq)"){ is =>
      var s = HashSet.empty[Int]
      for (i <- is) {
        val b = s; s += i; if (b eq s) ???
      }
      s
    },

    bm("immutable.BitSet (eq)"){ is =>
      var s = BitSet.empty
      for (i <- is) {
        val b = s; s += i; if (b eq s) ???
      }
      s
    }
  )

  val param = Param(Render.int, Editor.text, Parser.intsAsText)("Size", 1000)

  val guiSuite = GuiSuite(suite, param).describe(
    <.div(
      <.div(
        "This explores different ways of building a unique set of ", <.code("Int"),
        "s, and with the ability to react immediately when a duplicate is added."),
      <.div(^.marginTop := "1ex",
        "Included in the benchmarks are ", <.code("BitSet"), "s which are only useful when the numbers in the set:",
        <.ol(^.marginTop := "2px", ^.marginBottom := "0",
          <.li("Are ≥ 0.", <.em(" (else crash)")),
          <.li("Don't get too large.", <.em(" (time/memory scale with size of largest member)")))),
      linkToSource(sourceFilename))
  )
}