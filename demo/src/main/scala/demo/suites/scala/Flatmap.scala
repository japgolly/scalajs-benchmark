package demo.suites.scala

import demo.Util._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.Iso
import scala.collection.immutable._

object Flatmap {

  case class Params(size1: Int, size2: Int) {
    override val toString = s"$size1 * $size2"
  }

  type A = Int
  val a: A = 123

  private def makeBmFn(as: => TraversableOnce[A]): Benchmark.Fn =
    () => as.foreach(a => ())

  private def makeTravBm[C[x] <: Traversable[x]](name: String, fill: (Int, A) => C[A]): Benchmark[Params] =
    Benchmark.fromFn[Params](name) { p =>
      val flatOut = fill(p.size2, a)
      val input = fill(p.size1, a)
      makeBmFn(input flatMap (_ => flatOut))
    }

  private def makeIterBm[C[x] <: Traversable[x]](name: String, fill: (Int, A) => C[A]): Benchmark[Params] =
    Benchmark.fromFn[Params](name + ".iterator") { p =>
      val flatOut = fill(p.size2, a)
      val input = fill(p.size1, a)
      makeBmFn(input.toIterator flatMap (_ => flatOut))
    }

  def bm[C[x] <: Traversable[x]](name: String, fill: (Int, A) => C[A]): Vector[Benchmark[Params]] =
    Vector(makeTravBm(name, fill), makeIterBm(name, fill))

  val suite = {
    var bms = Vector.empty[Benchmark[Params]]
    bms ++= bm("List", List.fill(_)(_))
    bms ++= bm("Vector", Vector.fill(_)(_))
    bms ++= bm("Stream", Stream.fill(_)(_))
    bms ++= bm("Stack", Stack.fill(_)(_))
    bms ++= bm("Queue", Queue.fill(_)(_))
    Suite("FlatMap")(bms: _*)
  }

  val iso = Iso((m: Params) => Params.unapply(m).get)((Params.apply _).tupled)

  val param1 = GuiParam.int("Input Size", 100)
  val param2 = GuiParam.int("Flatmap Size", 50)
  val params = GuiParams.two(iso, param1, param2)

  val guiSuite = GuiSuite(suite, params).describe(
    <.div(<.div(^.marginBottom := "0.8em",
      "Inspects performance of a ", <.code("for (x <- values flatMap makeMoreValues) …"),
      "-like pattern with different collection types."),
      linkToSource(sourceFilename)))
}

//  def genericBM[C](name: String, fill: (Int, A) => C)(flatmap: (C, A => C) => C, foreach: (C, A => Unit) => Unit): Benchmark[Params] =
//    Benchmark.fromFn[Params](name) { p =>
//      val flatOut = fill(p.size2, a)
//      val input = fill(p.size1, a)
//      () => {
//        val c = flatmap(input, _ => flatOut)
//        foreach(c, _ => ())
//      }
//    }
//
// import scalaz._
// bms :+= genericBM("scalaz.IList", IList.fill(_)(_))(_ flatMap _, (c, f) => c.foldLeft(())((_, a) => f(a)))
// bms :+= genericBM("scalaz.DList", DList fromList List.fill(_)(_))(_ flatMap _, (c, f) => c.foldr(())((a, _) => f(a)))
