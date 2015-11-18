package japgolly.scalajs.benchmark.gui

import java.util.concurrent.TimeUnit

import japgolly.scalajs.benchmark.engine.Stats
import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.vdom.prefix_<^._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scalacss.ScalaCssReact._

/**
  * Format for a single value in [[Stats]].
  *
  * Eg. 32.456 sec
  */
case class StatValueFmt(render: Stats => ReactElement, getDouble: Stats => Option[Double])

object StatValueFmt {

  private val addThouRegex = """(\d)(?=(\d\d\d)+(?!\d))""".r

  def addThousandSeps(s: String): String = {
    def go(s: String)= addThouRegex.replaceAllIn(s, "$1,")
    s.indexOf('.') match {
      case n if n >= 0 =>
        val (a,b) = s.splitAt(n)
        go(a) + b
      case _ =>
        go(s)
    }
  }

  def duration(getUnits: FiniteDuration => Double, dp: Int): StatValueFmt = {
    val fmt = s"%.${dp}f"

    val tryGetUnits: Duration => Option[Double] = {
      case f: FiniteDuration => Some(getUnits(f))
      case _ => None
    }

    val toDouble: Stats => Option[Double] =
      s => tryGetUnits(s.average)

    def toString(od: Option[Double]): String =
      od.fold("∞")(d => addThousandSeps(fmt format d))

    val render: Stats => ReactElement =
      s => <.div(Styles.ResultTable.numericResult, toString(toDouble(s)))

    StatValueFmt(render, toDouble)
  }
}

/**
  * Format for a result derived from [[Stats]].
  *
  * Eg. ops/sec: 2058.8 ± 8.1
  *
  * @param score Formatter for the score itself.
  * @param error Formatter for the error in (score ± error).
  */
case class ResultFmt(header: String, score: StatValueFmt, error: StatValueFmt)

object ResultFmt {

  def abbrev(t: TimeUnit): String =
    t match {
      case TimeUnit.NANOSECONDS  => "ns"
      case TimeUnit.MICROSECONDS => "μs"
      case TimeUnit.MILLISECONDS => "ms"
      case TimeUnit.SECONDS      => "s"
      case TimeUnit.MINUTES      => "m"
      case TimeUnit.HOURS        => "d"
      case TimeUnit.DAYS         => "hr"
    }

  def getUnits(t: TimeUnit): FiniteDuration => Double =
    t match {
      case TimeUnit.NANOSECONDS  => _.toNanos.toDouble
      case TimeUnit.MICROSECONDS => _.toNanos.toDouble / 1000.0
      case TimeUnit.MILLISECONDS => _.toNanos.toDouble / 1000000.0
      case TimeUnit.SECONDS      => _.toMicros.toDouble / 1000000.0
      case TimeUnit.MINUTES      => _.toMillis.toDouble / 60000.0
      case TimeUnit.HOURS        => _.toMillis.toDouble / 3660000.0
      case TimeUnit.DAYS         => _.toSeconds.toDouble / (3660 * 24)
    }

  def duration(header: String, getUnits: FiniteDuration => Double, scoreDP: Int, errorDP: Int): ResultFmt =
    ResultFmt(
      header,
      StatValueFmt.duration(getUnits, scoreDP),
      StatValueFmt.duration(getUnits, errorDP))

  def opsPerT(t: TimeUnit, scoreDP: Int, errorDP: Int): ResultFmt = {
    val one = FiniteDuration(1, t)
    duration("ops/" + abbrev(t), one / _, scoreDP, errorDP)
  }

  def timePerOp(t: TimeUnit, scoreDP: Int, errorDP: Int): ResultFmt =
    duration(abbrev(t) + "/op", getUnits(t), scoreDP, errorDP)

  val OpsPerSec   = opsPerT(TimeUnit.SECONDS, 3, 0)
  val MillisPerOp = timePerOp(TimeUnit.MILLISECONDS, 3, 0)
  val MicrosPerOp = timePerOp(TimeUnit.MICROSECONDS, 3, 0)
}
