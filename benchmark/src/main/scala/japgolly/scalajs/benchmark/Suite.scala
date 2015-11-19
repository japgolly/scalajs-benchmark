package japgolly.scalajs.benchmark

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
final case class Suite[P](name: String, bms: Vector[Benchmark[P]])

final case class Plan[P](suite: Suite[P], params: Vector[P]) {
  @inline def name = suite.name
  @inline def bms = suite.bms

  val keys: List[PlanKey[P]] =
    for {
      (p, pi) <- params.iterator.zipWithIndex.toList
      (b, bi) <- bms.iterator.zipWithIndex
    } yield PlanKey(bi, pi)(b, p)

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