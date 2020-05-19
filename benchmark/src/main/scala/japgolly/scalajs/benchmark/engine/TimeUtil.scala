package japgolly.scalajs.benchmark.engine

import java.util.concurrent.TimeUnit
import monocle.Iso
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.scalajs.js

object TimeUtil {

  def toMs(d: Duration): Double = {
    d match {
      case f: FiniteDuration =>
        f.unit match {
          case TimeUnit.NANOSECONDS  => d.length.toDouble / 1000000.0
          case TimeUnit.MICROSECONDS => d.length.toDouble / 1000.0
          case TimeUnit.MILLISECONDS => d.length.toDouble
          case _                     => d.toMillis.toDouble
        }
      case _ => Double.NaN
    }
  }

  def fromMs(ms: Double): Duration =
    if (ms.isNaN)
      Duration.Undefined
    else
      FiniteDuration((ms * 1000000.0).toLong, TimeUnit.NANOSECONDS)

  val ms: Iso[Duration, Double] =
    Iso(toMs)(fromMs)

  private final val msFromSec: Double = 1000
  private final val msFromMin: Double = msFromSec * 60
  private final val msFromHr : Double = msFromMin * 60
  private final val msFromDay: Double = msFromHr * 24

  def getUnitsFromMs(t: TimeUnit): Double => Double =
    t match {
      case TimeUnit.NANOSECONDS  => _ * 1000000
      case TimeUnit.MICROSECONDS => _ * 1000
      case TimeUnit.MILLISECONDS => identity
      case TimeUnit.SECONDS      => _ / msFromSec
      case TimeUnit.MINUTES      => _ / msFromMin
      case TimeUnit.HOURS        => _ / msFromHr
      case TimeUnit.DAYS         => _ / msFromDay
    }

  def dateStrFromJsDate(d: js.Date): String =
    "%d%02d%02d".format(d.getFullYear(), d.getMonth() + 1, d.getDate())

  def timeStrFromJsDate(d: js.Date): String =
    "%02d%02d%02d".format(d.getHours(), d.getMinutes(), d.getSeconds())

}
