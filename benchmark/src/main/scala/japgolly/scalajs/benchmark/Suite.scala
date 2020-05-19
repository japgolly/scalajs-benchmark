package japgolly.scalajs.benchmark

import monocle.Lens

/**
  * A suite of benchmarks.
  *
  * If you want to use this suite in the GUI, you'll need a [[japgolly.scalajs.benchmark.gui.GuiSuite]] which combines
  * this with some extra data needed just for the GUI.
  *
  * Benchmarks take parameter data (or if they don't need any, then they'll take `Unit`).
  * To run a suite of benchmarks (without using a GUI), combine this with your desired param values to form a [[Plan]],
  * and then pass it to one of the run methods in [[japgolly.scalajs.benchmark.engine.Engine]].
  */
final class Suite[-P](val name: String, val bms: Vector[Benchmark[P]]) {

  lazy val filenameFriendlyName: String =
    name.replaceAll("[ .]", "_")
}

object Suite {
  def apply[P](name: String, bms: Vector[Benchmark[P]]): Suite[P] =
    new Suite(name, bms)

  def apply[P](name: String)(bms: Benchmark[P]*): Suite[P] =
    Suite(name, bms.toVector.sortBy(_.name))

  def bms[P]: Lens[Suite[P], Vector[Benchmark[P]]] =
    Lens((_: Suite[P]).bms)(b => s => Suite(s.name, b))
}

final case class Plan[P](suite: Suite[P], params: Vector[P]) {
  @inline def name = suite.name
  @inline def bms = suite.bms

  val keys: List[PlanKey[P]] =
    // Don't reorder this by bm
    // Jumping back and forth between BMs reduces unrealistic over-optimisation and theoretical over-fitting
    (for {
      (p, pi) <- params.iterator.zipWithIndex
      (b, bi) <- bms.iterator.zipWithIndex
    } yield PlanKey(bi, pi)(b, p))
      .toList

  val totalBenchmarks: Int =
    keys.length
}

/**
  * Unique key for a (benchmark, param) tuple in a [[Plan]].
  *
  * Universal equality holds; it can be used as a map key.
  */
final case class PlanKey[P](bmIndex: Int, paramIndex: Int)
                           (val bm: Benchmark[P], val param: P)