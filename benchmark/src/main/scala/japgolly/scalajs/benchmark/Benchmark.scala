package japgolly.scalajs.benchmark

import Benchmark.SetupFn

/**
  * A benchmark. A single procedure to measure.
  *
  * @param setup Prepare the benchmark function. Setup and teardown times aren't measured.
  * @tparam P A param that alters the benchmark / data used by the benchmark.
  */
final class Benchmark[-P](val name: String, val setup: SetupFn[P], val isDisabledByDefault: Boolean) {
  override def toString = name

  // TODO Having disabledByDefault in Benchmark is a hack
  // It only makes sense for the GUI package, no?
  def setDisabledByDefault: Benchmark[P] =
    new Benchmark(name, setup, true)
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
    new Benchmark(name, Setup.unit(() => f), false)

  /**
    * Creates a benchmark that accepts a parameter (without needing transformation or preprocessing).
    */
  def apply[A](name: String, f: A => Any): Benchmark[A] =
    new Benchmark(name, Setup(a => () => f(a)), false)

  def fromFn[A](name: String)(f: A => Fn): Benchmark[A] =
    new Benchmark(name, Setup(f), false)

  def setup[A, B](prepare: A => B): Builder[A, B] =
    new Builder[A, B](prepare)

  class Builder[A, B](val prepare: A => B) {
    def apply(name: String)(f: B => Any): Benchmark[A] =
      new Benchmark(name, Setup { a =>
        val b = prepare(a)
        () => f(b)
      }, false)
    def map[C](f: B => C): Builder[A, C] =
      new Builder(f compose prepare)
  }}
