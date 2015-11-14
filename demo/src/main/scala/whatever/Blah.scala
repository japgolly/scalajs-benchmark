package whatever

import scala.collection.immutable._
import scala.collection.mutable
import Benchy._

object IntSet_X {

  lazy val suite: Suite[Int] = {

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
      }),

      Benchmark[Int]("mutableBitSetAdd", setup2 { is =>
        val s = mutable.BitSet.empty
        for (i <- is) if (!s.add(i)) ???
        s
      })

    )

//    Suite("IntSet", bms, Vector(10, 100, 1000, 10000))
    Suite("IntSet", bms, Vector(10, 100, 1000))

  }
}