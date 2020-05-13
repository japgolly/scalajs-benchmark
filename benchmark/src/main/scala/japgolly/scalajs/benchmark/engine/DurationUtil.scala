package japgolly.scalajs.benchmark.engine

import java.util.concurrent.TimeUnit
import monocle.Iso
import scala.concurrent.duration.FiniteDuration

object DurationUtil {

  def toMs(d: FiniteDuration): Double = {
    d.unit match {
      case TimeUnit.NANOSECONDS  => d.length.toDouble / 1000000.0
      case TimeUnit.MICROSECONDS => d.length.toDouble / 1000.0
      case TimeUnit.MILLISECONDS => d.length.toDouble
      case _                     => d.toMillis.toDouble
    }
  }

  def fromMs(ms: Double): FiniteDuration =
    FiniteDuration((ms * 1000000.0).toLong, TimeUnit.NANOSECONDS)

  val ms: Iso[FiniteDuration, Double] =
    Iso(toMs)(fromMs)
}

