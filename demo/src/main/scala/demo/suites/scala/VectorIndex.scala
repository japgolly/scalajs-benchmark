package demo.suites.scala

import demo.Util._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.Iso
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

  private def bh(o: Option[Int]): Unit = ()

  val suite = Suite("Vector index")(

    bm("try/catch")(d => bh(
      try
        Some(d.vector(d.index))
      catch {
        case _: IndexOutOfBoundsException => None
      }
    )),

    bm("check length")(d => bh(
      if (d.index >= 0 && d.index < d.vector.length)
        Some(d.vector(d.index))
      else
        None
    )),

    bm("lift")(d => bh(
      d.vector.lift(d.index)
    ))
  )

  val iso = Iso((m: Cfg) => Cfg.unapply(m).get)((Cfg.apply _).tupled)

  val param1 = Param(Render.int, Editor.text, Parser.intsAsText)("Size", 50)
  val param2 = Param(Render.int, Editor.text, Parser.intsAsText)("Index", 0, 100)
  val params = Params.two(iso, param1, param2)

  val guiSuite = GuiSuite(suite, params).describe(
    <.div(<.div(^.marginBottom := "0.8em",
      "Experiments with different ways of reading a ", <.code("Vector[T]"),
      " element by index, returning an ", <.code("Option[T]"), "."),
      linkToSource(sourceFilename)))
}