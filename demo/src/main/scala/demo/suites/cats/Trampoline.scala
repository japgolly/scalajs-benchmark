package demo.suites.cats

import japgolly.scalajs.benchmark._, gui._
import cats._
import cats.implicits._
import cats.free.Trampoline

object TrampolineBM {

  def evalFib(n: Int): Eval[Int] =
    if (n < 2) Eval.now(n)
    else for {
      x <- Eval.defer(evalFib(n - 1))
      y <- Eval.defer(evalFib(n - 2))
    } yield x + y

  def trampolineFib(n: Int): Trampoline[Int] =
    if (n < 2) Trampoline.done(n)
    else for {
      x <- Trampoline.defer(trampolineFib(n - 1))
      y <- Trampoline.defer(trampolineFib(n - 2))
    } yield x + y

  val suite = Suite[Int]("Trampoline")(
    Benchmark("eval",       evalFib(_).value),
    Benchmark("trampoline", trampolineFib(_).run))

  val param = GuiParam.int("Size", 15, 30)

  val guiSuite = GuiSuite(suite, param)
}
