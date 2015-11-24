package demo.suites.shootouts

import demo.Util._
import demo.suites
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import monocle.Iso

object FreeMonadShootout {

  type BM = Benchmark[Int]
  sealed abstract class Lib(val name: String) {
    def free: BM
    def freeCoyo: BM
  }

  object Cats extends Lib("Cats") {
    override def free     = suites.cats.FreeMonads.FreeK.bmFn0FoldMap
    override def freeCoyo = suites.cats.FreeMonads.Coyo.bmFn0FoldMap
  }

  object Scalaz extends Lib("Scalaz") {
    override def free     = suites.scalaz.FreeMonads.FreeK.bmFn0FoldMap
    override def freeCoyo = suites.scalaz.FreeMonads.Coyo.bmFn0FoldMap
  }

  case class Params(lib: Lib, size: Int) {
    override def toString = s"$lib @ $size"
  }

  val param1 = GuiParam.enum[Lib]("Library", Seq(Cats, Scalaz))(_.name)
  val param2 = GuiParam.int("Size", 500)

  val iso = Iso((m: Params) => Params.unapply(m).get)((Params.apply _).tupled)
  val params = GuiParams.two(iso, param1, param2)

  val suite = Suite[Params]("Free monads")(
    Benchmark.derive("Free",               (_: Params).lib.free    )(_.size),
    Benchmark.derive("Free with coYoneda", (_: Params).lib.freeCoyo)(_.size))

  val guiSuite = GuiSuite(suite, params).describe(
    <.div(
      <.div("Shootout between free monad implementations."),
      linkToSource(sourceFilename)))
}
