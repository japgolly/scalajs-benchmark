package japgolly.scalajs.benchmark.engine

import scala.scalajs.js

/**
  * @param rawData Times, in milliseconds, of each execution.
  */
final case class IterationStats(rawData: js.Array[Double])

object IterationStats {

  final class Builder {
    private val _rawData   = new js.Array[Double]
    private var _totalTime = 0.0

    def add(d: Double): Unit = {
      _rawData.push(d)
      _totalTime += d
    }

    def totalTime() =
      _totalTime

    def result(): IterationStats =
      IterationStats(_rawData)
  }

}
