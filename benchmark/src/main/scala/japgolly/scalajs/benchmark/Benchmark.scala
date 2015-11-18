package japgolly.scalajs.benchmark

import Benchmark.SetupFn

/**
  * A benchmark. A single procedure to measure.
  *
  * @param setup Prepare the benchmark function. Setup and teardown times aren't measured.
  * @tparam P A param that alters the benchmark / data used by the benchmark.
  */
final class Benchmark[-P](val name: String, val setup: SetupFn[P]) {
  override def toString = name
}

object Benchmark {

  /**
    * A function that performs the part of the benchmark which should be timed.
    *
    * The timer starts immediately before running this and stop immediately on completion.
    */
  type Fn = () => Any

  type SetupFn[-P] = Setup[P, Fn]
  // TODO later: type SetupFn[-I] = Setup[I, Setup[Unit, Fn]]

  /**
    * Creates a benchmark that doesn't need any external data.
    */
  def apply(name: String)(f: => Any): Benchmark[Unit] =
    new Benchmark(name, Setup.unit(() => f))

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
}
