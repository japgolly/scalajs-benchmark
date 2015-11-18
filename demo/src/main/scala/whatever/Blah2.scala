package whatever.newshit

import monocle.Iso
import scala.collection.immutable._
import scala.collection.mutable
import scala.util.Try
import scalaz._, Scalaz._
import japgolly.scalajs.benchmark._
import Benchmark.SetupFn
import gui._
import Param._

object IntSet_X {

  lazy val suite = {

    val setup = SetupFn.map[Int, List[Int]](size =>
      // Puts it in a non-linear, deterministic order then change to disrupt hash order
      (-size to -1).toSet.iterator.map(-(_: Int)).toList)

    Suite[Int]("IntSet", Vector(

      // TODO setup("immutableSet"){ is =>
      Benchmark("immutableSet")(setup { is =>
        var s = Set.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }),

      Benchmark("mutableBitSetAdd")(setup { is =>
        val s = mutable.BitSet.empty
        for (i <- is) if (!s.add(i)) ???
        s
      })

    ))
  }

  val param = Param(Render.int, Editor.text, Parser.intsAsText)("size", 10, 100)

  lazy val guiSuite = GuiSuite(suite, param)
}

// =====================================================================================================================

object IntSet_X2 {

  case class Multi(size: Int, reverse: Boolean) {
    override def toString = s"$size | $reverse"
  }

  lazy val suite = {

    val setup = SetupFn.map[Multi, List[Int]] { p =>
      val x = (-p.size to -1).toSet.iterator.map(-(_: Int)).toList
      if (p.reverse) x.reverse else x
    }

    Suite[Multi]("IntSet₂", Vector(

      // TODO setup("immutableSet"){ is =>
      Benchmark("immutableSet")(setup { is =>
        var s = Set.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }),

      Benchmark("mutableBitSetAdd")(setup { is =>
        val s = mutable.BitSet.empty
        for (i <- is) if (!s.add(i)) ???
        s
      })

    ))
  }

  val param1 = Param(Render.int, Editor.text, Parser.intsAsText)("size", 5, 10)
  val param2 = Param(Render.bool, Editor.text, Parser.boolsAsText)("reverse", true, false)

  val iso = Iso((m: Multi) => Multi.unapply(m).get)((Multi.apply _).tupled)
  val params = Params.two(iso, param1, param2)

  lazy val guiSuite = GuiSuite(suite, params)
}