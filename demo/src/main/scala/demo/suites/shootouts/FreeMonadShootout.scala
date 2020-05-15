package demo.suites.shootouts

import demo.Util._
import demo.{Libraries, suites}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import monocle.macros.GenIso

object FreeMonadShootout {

  type BM = Benchmark[Int]
  sealed abstract class Lib(val name: String) {
    def freeFoldMap: BM
    def freeMapSuspension: BM
  }

  object Cats extends Lib(Libraries.Cats.fullName) {
    override def freeFoldMap       = suites.cats.FreeMonads.bmFn0FoldMap
    override def freeMapSuspension = suites.cats.FreeMonads.bmFn0Compile
  }

  object Scalaz extends Lib(Libraries.Scalaz.fullName) {
    override def freeFoldMap       = suites.scalaz.FreeMonads.bmFn0FoldMap
    override def freeMapSuspension = suites.scalaz.FreeMonads.bmFn0MapSuspension
  }

  case class Params(lib: Lib, size: Int) {
    override def toString = s"${lib.name} @ $size"
  }

  val param1 = GuiParam.enum[Lib]("Library", Cats, Scalaz)(_.name)
  val param2 = GuiParam.int("Size", 500)

  val iso = GenIso.fields[Params]
  val params = GuiParams.combine2(iso)(param1, param2)

  val suite = Suite[Params]("Free monads")(
    Benchmark.derive("Free --> Fn0 (foldMap)",       (_: Params).lib.freeFoldMap      )(_.size),
    Benchmark.derive("Free --> Fn0 (mapSuspension)", (_: Params).lib.freeMapSuspension)(_.size))

  val guiSuite = GuiSuite(suite, params).describe(
    <.div(
      <.div("Shootout between free monad implementations."),
      linkToSource(sourceFilename)))
}
