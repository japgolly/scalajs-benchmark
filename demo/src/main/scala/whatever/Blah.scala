package whatever

import scala.collection.immutable._
import scala.collection.mutable
import Benchy._

object IntSet_X {

  lazy val suite: Suite = {

    def setup(size: Int): List[Int] =
      // Puts it in a non-linear, deterministic order then change to disrupt hash order
      (-size to -1).toSet.iterator.map(-(_: Int)).toList

    type A = Int
    type B = List[Int]

    def setup2(f: B => Any): SetupFn[A] =
      a => {
        val b = setup(a)
        () => f(b)
      }

    val bms = Vector[Benchmark[Int]](

      Benchmark[Int]("immutableSet", setup2 { is =>
        var s = Set.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      })
,
      Benchmark[Int]("mutableBitSetAdd", setup2 { is =>
        val s = mutable.BitSet.empty
        for (i <- is) if (!s.add(i)) ???
        s
      })
    )

//    Suite("IntSet", bms, Vector(10, 100, 1000, 10000))
//    Suite("IntSet", bms, Vector(10, 100, 1000))
    Suite("IntSet", bms, Vector(10, 100))
  }
}

object IntSet_X2 {

  case class Params(size: Int, reverse: Boolean)

  lazy val suite: Suite = {

    def setup(p: Params): List[Int] = {
      // Puts it in a non-linear, deterministic order then change to disrupt hash order
      val x = (-p.size to -1).toSet.iterator.map(-(_: Int)).toList
      if (p.reverse) x.reverse else x
    }

    type B = List[Int]

    def setup2(f: B => Any): SetupFn[Params] =
      a => {
        val b = setup(a)
        () => f(b)
      }

    val bms = Vector[Benchmark[Params]](

      Benchmark("immutableSet", setup2 { is =>
        var s = Set.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      })
,
      Benchmark("mutableBitSetAdd", setup2 { is =>
        val s = mutable.BitSet.empty
        for (i <- is) if (!s.add(i)) ???
        s
      })
    )

//    Suite("IntSet", bms, Vector(10, 100, 1000, 10000))
//    Suite("IntSet", bms, Vector(10, 100, 1000))

    val params =
      for {
        s <- Vector(10, 100)
        r <- Vector(false, true)
      } yield Params(s, r)

    Suite("IntSet₂", bms, params)
  }
}