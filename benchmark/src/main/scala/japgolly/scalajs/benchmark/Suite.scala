package japgolly.scalajs.benchmark

final case class Suite[P](name: Name, bms: Vector[Benchmark[P]])

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

final case class PlanKey[P](bmIndex: Int, paramIndex: Int)
                           (val bm: Benchmark[P], val param: P)