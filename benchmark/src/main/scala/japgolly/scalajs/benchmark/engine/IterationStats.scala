package japgolly.scalajs.benchmark.engine

final case class IterationStats(samples: Int, sumMs: Double) {
  val mean = sumMs / samples.toDouble
}

object IterationStats {

  final class Builder {
    private var _samples = 0
    private var _sum = 0.0

    def add(d: Double): Unit = {
      _samples += 1
      _sum += d
    }

    def totalTime() =
      _sum

    def result(): IterationStats =
      IterationStats(_samples, _sum)
  }

}
