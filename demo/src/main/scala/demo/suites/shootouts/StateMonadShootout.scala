package demo.suites.shootouts

import demo.Libraries
import demo.Util._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import monocle.macros.GenIso

object StateMonadShootout {

  type BM = Benchmark[Int]

  sealed abstract class Lib(val name: String) {
    def bm: BM
  }

  // ===================================================================================================================

  object Cats extends Lib(Libraries.Cats.fullName) {
    import cats.data.State

    val setup = Benchmark.setup[Int, State[Int, Unit]] { size =>
      val step = State[Int, Unit](s => (s + 1, ()))
      Iterator.fill(size)(step).reduce((a, b) => a.flatMap(_ => b))
    }

    val bm = setup("")(_.runS(0).value: Int)
  }

  // ===================================================================================================================

  object Scalaz extends Lib(Libraries.Scalaz.fullName) {
    import scalaz.State

    val setup = Benchmark.setup[Int, State[Int, Unit]] { size =>
      val step = State[Int, Unit](s => (s + 1, ()))
      Iterator.fill(size)(step).reduce((a, b) => a.flatMap(_ => b))
    }

    val bm = setup("")(_.exec(0): Int)
  }

  // ===================================================================================================================

  object ScalazTramp extends Lib(Libraries.Scalaz.fullName + " (trampolined)") {
    import scalaz.{StateT, Trampoline}
    import scalaz.Free.Trampoline

    val setup = Benchmark.setup[Int, StateT[Trampoline, Int, Unit]] { size =>
      val step = StateT[Trampoline, Int, Unit](s => Trampoline.done(s + 1, ()))
      Iterator.fill(size)(step).reduce((a, b) => a.flatMap(_ => b))
    }

    val bm = setup("")(_.exec(0).run: Int)
  }

  // ===================================================================================================================

  case class Params(lib: Lib, size: Int) {
    override def toString = s"${lib.name} @ $size"
  }
  val param1 = GuiParam.enum[Lib]("Library", Cats, Scalaz, ScalazTramp)(_.name)
  val param2 = GuiParam.int("Size", 100, 1000, 10000)

  val iso = GenIso.fields[Params]
  val params = GuiParams.combine2(iso)(param1, param2)

  val suite = Suite[Params]("State monads")(
    Benchmark.derive("Increment state by 1.", (_: Params).lib.bm)(_.size))

  val guiSuite = GuiSuite(suite, params).describe(
    <.div(
      <.div("Shootout between state monad implementations."),
      linkToSource(sourceFilename)))
}
