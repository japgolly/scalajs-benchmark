package whatever

import monocle.Iso
import scala.collection.immutable._
import scala.collection.mutable
import japgolly.scalajs.benchmark._
import Benchmark.SetupFn
import gui._
import Param._

object Demos {

  lazy val noParams = {

    val suite = Suite[Unit]("No Params", Vector(

      Benchmark("immutableSet") {
        var s = Set.empty[Int]
        for (i <- 1 to 100) if (s contains i) ??? else s += i
        s
      },

      Benchmark("mutableBitSetAdd") {
        val s = mutable.BitSet.empty
        for (i <- 1 to 100) if (!s.add(i)) ???
        s
      }
    ))

    GuiSuite(suite)
  }

  // =====================================================================================================================

  lazy val oneParam = {

    val bm = SetupFn.map[Int, List[Int]](size =>
      (-size to -1).toSet.iterator.map(-(_: Int)).toList)

    val suite = Suite[Int]("One Param", Vector(

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
    ))

    val param = Param(Render.int, Editor.text, Parser.intsAsText)("Size", 10, 100)

    GuiSuite(suite, param)
  }

  // =====================================================================================================================

  case class Multi(size: Int, reverse: Boolean) {
    override def toString = s"$size | $reverse"
  }

  lazy val twoParams = {

    val bm = SetupFn.map[Multi, List[Int]] { p =>
      val x = (-p.size to -1).toSet.iterator.map(-(_: Int)).toList
      if (p.reverse) x.reverse else x
    }

    val suite = Suite[Multi]("Two Params", Vector(

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
    ))

    val param1 = Param(Render.int, Editor.text, Parser.intsAsText)("Size", 5, 10)
    val param2 = Param(Render.bool, Editor.text, Parser.boolsAsText)("Reverse", true, false)

    val iso = Iso((m: Multi) => Multi.unapply(m).get)((Multi.apply _).tupled)
    val params = Params.two(iso, param1, param2)

    GuiSuite(suite, params)
  }

}