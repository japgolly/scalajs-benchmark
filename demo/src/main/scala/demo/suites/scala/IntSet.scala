package demo.suites.scala

import scala.collection.immutable._
import scala.collection.mutable

import japgolly.scalajs.benchmark._
import Benchmark.SetupFn
import gui._
import Param._

object IntSet {

  val bm = SetupFn.map[Int, List[Int]](size =>
    // Puts it in a non-linear, deterministic order then change to disrupt hash order
    (-size to -1).toSet.iterator.map(-(_: Int)).toList)

  val suite = Suite("Int Set")(

    bm("immutableSet"){ is =>
      var s = Set.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("mutableSet"){ is =>
      val s = mutable.Set.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("intMap"){ is =>
      var s = IntMap.empty[Unit]
      for (i <- is) if (s contains i) ??? else s = s.updated(i, ())
      s
    },

//      bm("listSet"){ is =>
//        var s = ListSet.empty[Int]
//        for (i <- is) if (s contains i) ??? else s += i
//        s
//      }),

    bm("immutableHashSet"){ is =>
      var s = HashSet.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("mutableHashSet"){ is =>
      val s = mutable.HashSet.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("immutableTreeSet"){ is =>
      var s = TreeSet.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("mutableTreeSet"){ is =>
      val s = mutable.TreeSet.empty[Int]
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("immutableBitSet"){ is =>
      var s = BitSet.empty
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("mutableBitSet"){ is =>
      val s = mutable.BitSet.empty
      for (i <- is) if (s contains i) ??? else s += i
      s
    },

    bm("mutableBitSetAdd"){ is =>
      val s = mutable.BitSet.empty
      for (i <- is) if (!s.add(i)) ???
      s
    },

    // ===============================================================================================================
    // Add then check by ref

    bm("immutableSetEq"){ is =>
      var s = Set.empty[Int]
      for (i <- is) {
        val b = s; s += i; if (b eq s) ???
      }
      s
    },

//      bm("listSetEq"){ is =>
//        var s = ListSet.empty[Int]
//        for (i <- is) {
//          val b = s; s += i; if (b eq s) ???
//        }
//        s
//      }),

    bm("immutableHashSetEq"){ is =>
      var s = HashSet.empty[Int]
      for (i <- is) {
        val b = s; s += i; if (b eq s) ???
      }
      s
    },

    bm("immutableBitSetEq"){ is =>
      var s = BitSet.empty
      for (i <- is) {
        val b = s; s += i; if (b eq s) ???
      }
      s
    }
  )

  val param = Param(Render.int, Editor.text, Parser.intsAsText)("Size", 1000)

  val guiSuite = GuiSuite(suite, param)
}