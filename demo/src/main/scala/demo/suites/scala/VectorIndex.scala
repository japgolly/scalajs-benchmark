package demo.suites.scala

import demo.Util._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.GenIso
import scala.collection.immutable._

object VectorIndex {

  case class Cfg(vectorSize: Int, index: Int) {
    override def toString = s"$vectorSize($index)"
  }

  case class Data(vector: Vector[Int], index: Int)

  val bm = Benchmark.setup[Cfg, Data](cfg =>
    Data(
      Vector.fill(cfg.vectorSize)(0),
      cfg.index))

  @inline private def ensureType(o: Option[Int]) = o

  val suite = Suite("Vector index")(

    bm("try/catch")(d => ensureType(
      try
        Some(d.vector(d.index))
      catch {
        case _: IndexOutOfBoundsException => None
      }
    )),

    bm("check length")(d => ensureType(
      if (d.index >= 0 && d.index < d.vector.length)
        Some(d.vector(d.index))
      else
        None
    )),

    bm("lift")(d => ensureType(
      d.vector.lift(d.index)
    ))
  )

  val iso = GenIso.fields[Cfg]

  val param1 = GuiParam.int("Size", 50)
  val param2 = GuiParam.int("Index", 0, 100)
  val params = GuiParams.combine2(iso)(param1, param2)

  val guiSuite = GuiSuite(suite, params).describe(
    <.div(<.div(^.marginBottom := "0.8em",
      "Experiments with different ways of reading a ", <.code("Vector[T]"),
      " element by index, returning an ", <.code("Option[T]"), "."),
      linkToSource(sourceFilename)))
}