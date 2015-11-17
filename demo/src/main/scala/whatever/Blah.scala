package whatever

import whatever.BMComp.Suite2
import whatever.Formaty._
import scala.collection.immutable._
import scala.collection.mutable
import Benchy._
import NotMyProb._

import scala.util.Try
import scalaz._, Scalaz._

object IntSet_X {

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

  val param1 = Param[Int, String]("size", renderInt, textEditor, Some("10, 100"))
  object paramz extends Params[Int] {
    type P = Int

    override val paramDefs: Vector[ParamWithKey[P]] =
      Vector(
        new ParamWithKey[Int] {
          override type B = String
          override val param = param1
          override val key = Key[B]()

          override def parseEditorState(ob: Option[B]): String \/ Vector[Int] =
          ob match {
            case None => \/-(Vector.empty)
            case Some(b) =>
              b.split("[ ,]")
                .iterator
                .map(_.trim)
                .filter(_.nonEmpty)
                .map(is => \/.fromTryCatchNonFatal(is.toInt).leftMap(_ => s"$is is not a valid integer."))
                .toVector
                .sequenceU
          }
        }
      )

    override val forState =
      (s: GenState) => {
        val p1 = paramDefs.head
        val x = p1.parseEditorState(p1.key.get(s))
        x leftMap (_ => p1)
      }
  }

  lazy val suite2 = Suite2(suite)(paramz)
}

// =====================================================================================================================

/*
object IntSet_X2 {

  case class Params(size: Int, reverse: Boolean) {
    override def toString = s"$size | $reverse"
  }

  lazy val suite = {

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

//      Benchmark("immutableSet", setup2 { is =>
//        var s = Set.empty[Int]
//        for (i <- is) if (s contains i) ??? else s += i
//        s
//      })
//      ,
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

  lazy val suite2 = Suite2(suite)(
    FmtParam.int("size").cmap[Params](_.size) :+
      FmtParam.bool("reverse").cmap[Params](_.reverse)
  )
}*/