package japgolly.scalajs.benchmark

import Benchmark.SetupFn

final class Benchmark[-A](val name: Name, val setup: SetupFn[A]) {
  override def toString = name
}

object Benchmark {

  def apply(name: Name)(f: => Any): Benchmark[Unit] =
    new Benchmark(name, Setup.unit(() => f))

  type SetupFn[-I] = Setup[I, Fn]
  // TODO later: type SetupFn[-I] = Setup[I, Setup[Unit, Fn]]

  object SetupFn {

    def apply[A](f: A => Any): SetupFn[A] =
      Setup(a => () => f(a))

    def map[A, B](prepare: A => B): Builder[A, B] =
      new Builder[A, B](
        f => Setup { a =>
          val b = prepare(a)
          () => f(b)
        }
      )

    class Builder[A, B](g: (B => Any) => SetupFn[A]) {
      def apply(name: String)(f: B => Any): Benchmark[A] =
        new Benchmark(name, g(f))
    }
  }

  /**
    * A function that performs the part of the benchmark which should be timed.
    *
    * The timer starts immediately before running this and stop immediately on completion.
    */
  type Fn = () => Any

}

final class Setup[-A, +B](val run: A => (B, Teardown)) extends AnyVal

object Setup {
  def empty[A]: Setup[A, A] =
    apply(identity)

  def unit[A](a: A): Setup[Unit, A] =
    new Setup(Function const ((a, Teardown.empty)))

  /** Setup only; no teardown. */
  def apply[A, B](f: A => B): Setup[A, B] =
    andTeardown(f, Teardown.empty)

  def andTeardown[A, B](f: A => B, t: Teardown): Setup[A, B] =
    new Setup(a => (f(a), t))

  def andTeardown[A, B](f: A => (B, Teardown)): Setup[A, B] =
    new Setup(f)
}

final class Teardown(val run: () => Unit) extends AnyVal
object Teardown {

  def apply(f: => Unit): Teardown =
    new Teardown(() => f)

  val empty: Teardown =
    Teardown(() => ())
}