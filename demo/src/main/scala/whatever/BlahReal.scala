package whatever

import scala.collection.immutable._
import scala.collection.mutable

/*
object BlahReal_IntSet {

  lazy val suite = {

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

      // ===================================================================================================================

      Benchmark("immutableSet", setup2 { is =>
        var s = Set.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }),

      Benchmark("mutableSet", setup2 { is =>
        val s = mutable.Set.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }),

      Benchmark("intMap", setup2 { is =>
        var s = IntMap.empty[Unit]
        for (i <- is) if (s contains i) ??? else s = s.updated(i, ())
        s
      }),

//      Benchmark("listSet", setup2 { is =>
//        var s = ListSet.empty[Int]
//        for (i <- is) if (s contains i) ??? else s += i
//        s
//      }),

      Benchmark("immutableHashSet", setup2 { is =>
        var s = HashSet.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }),

      Benchmark("mutableHashSet", setup2 { is =>
        val s = mutable.HashSet.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }),

      Benchmark("immutableTreeSet", setup2 { is =>
        var s = TreeSet.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }),

      Benchmark("mutableTreeSet", setup2 { is =>
        val s = mutable.TreeSet.empty[Int]
        for (i <- is) if (s contains i) ??? else s += i
        s
      }),

      Benchmark("immutableBitSet", setup2 { is =>
        var s = BitSet.empty
        for (i <- is) if (s contains i) ??? else s += i
        s
      }),

      Benchmark("mutableBitSet", setup2 { is =>
        val s = mutable.BitSet.empty
        for (i <- is) if (s contains i) ??? else s += i
        s
      }),

      Benchmark("mutableBitSetAdd", setup2 { is =>
        val s = mutable.BitSet.empty
        for (i <- is) if (!s.add(i)) ???
        s
      }),

      // ===================================================================================================================
      // Add then check by ref

      Benchmark("immutableSetEq", setup2 { is =>
        var s = Set.empty[Int]
        for (i <- is) {
          val b = s; s += i; if (b eq s) ???
        }
        s
      }),

//      Benchmark("listSetEq", setup2 { is =>
//        var s = ListSet.empty[Int]
//        for (i <- is) {
//          val b = s; s += i; if (b eq s) ???
//        }
//        s
//      }),

      Benchmark("immutableHashSetEq", setup2 { is =>
        var s = HashSet.empty[Int]
        for (i <- is) {
          val b = s; s += i; if (b eq s) ???
        }
        s
      }),

      Benchmark("immutableBitSetEq", setup2 { is =>
        var s = BitSet.empty
        for (i <- is) {
          val b = s; s += i; if (b eq s) ???
        }
        s
      })
    ).sortBy(_.name)

    //    Suite("IntSet", bms, Vector(10, 100, 1000, 10000))
    //    Suite("IntSet", bms, Vector(10, 100, 1000))
    Suite("IntSet", bms, Vector(100))
  }

  lazy val suite2 = Suite2(suite)(FmtParam int "size")
}
*/