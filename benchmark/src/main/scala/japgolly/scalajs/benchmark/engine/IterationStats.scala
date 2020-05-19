package japgolly.scalajs.benchmark.engine

/**
  * @param sum Purposely vague. In practice it's usually duration in ms.
  */
final case class IterationStats(samples: Int, sum: Double) {
  val mean = sum / samples.toDouble

  def modifyMean(f: Double => Double): IterationStats =
    IterationStats(samples = 1, sum = f(mean))
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
