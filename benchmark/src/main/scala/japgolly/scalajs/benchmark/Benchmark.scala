package japgolly.scalajs.benchmark

import Benchmark.SetupFn

final case class Benchmark[-I](name: Name, setup: SetupFn[I]) {
  override def toString = name.value
}

object Benchmark {

  type SetupFn[-I] = Setup[I, Fn]
  // TODO later: type SetupFn[-I] = Setup[I, Setup[Unit, Fn]]

  /**
    * A function that performs the part of the benchmark which should be timed.
    *
    * The timer starts immediately before running this and stop immediately on completion.
    */
  type Fn = () => Any

}

final case class Name(value: String) extends AnyVal

final case class Setup[-A, +B](run: A => (B, Teardown)) extends AnyVal

final case class Teardown(run: () => Unit) extends AnyVal