package whatever

import java.util.concurrent.TimeUnit
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import whatever.Benchy._
import scala.concurrent.duration._
import scalacss.ScalaCssReact._

object Formaty {

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
  def mkDouble(t: TimeUnit): FiniteDuration => Double =
    t match {
      case TimeUnit.NANOSECONDS  => _.toNanos.toDouble
      case TimeUnit.MICROSECONDS => _.toNanos.toDouble / 1000.0
      case TimeUnit.MILLISECONDS => _.toNanos.toDouble / 1000000.0
      case TimeUnit.SECONDS      => _.toMicros.toDouble / 1000000.0
      case TimeUnit.MINUTES      => _.toMillis.toDouble / 60000.0
      case TimeUnit.HOURS        => _.toMillis.toDouble / 3660000.0
      case TimeUnit.DAYS         => _.toSeconds.toDouble / (3660 * 24)
    }

  trait ValueFmt {
    def render(s: RunStats): ReactElement
    def asDouble(s: RunStats): Option[Double]
  }

  case class ResultFmt(header: String,
                       fmtScore: ValueFmt,
                       fmtMoE: ValueFmt)

  object ResultFmt {
    def fmtDurToDbl(fmtF: FiniteDuration => Double, dp: Int): ValueFmt =
      new ValueFmt {
        val fmt = if (dp <= 0) "%0f" else s"%0.${dp}f"

        def fmtD(avgOpDuration: Duration): Option[Double] =
          avgOpDuration match {
            case f: FiniteDuration => Some(fmtF(f))
            case _ => None
          }

        def fmtS(od: Option[Double]): String =
          od.fold("∞")(scoreToString)

        def scoreToString(d: Double) = fmt format d

        override def asDouble(s: RunStats) =
          fmtD(s.average)

        override def render(s: RunStats): ReactElement =
          <.div(Styles.ResultTable.numericResult, fmtS(asDouble(s)))
      }

    val fmtError: ValueFmt = new ValueFmt {
      override def asDouble(s: RunStats) = None
      override def render(s: RunStats): ReactElement = <.div("?")
    }

    def OpsPerT(t: TimeUnit, dp: Int): ResultFmt = {
      val one = FiniteDuration(1, t)
      val hdr = "ops/" + abbrev(t)
      val fmtScore = fmtDurToDbl(one / _, dp)
      ResultFmt(hdr, fmtScore, fmtError)
    }

    def TPerOp(t: TimeUnit, dp: Int): ResultFmt = {
      val hdr = abbrev(t) + "/op"
      val fmtScore = fmtDurToDbl(mkDouble(t), dp)
      ResultFmt(hdr, fmtScore, fmtError)
    }

    val OpsPerSec   = OpsPerT(TimeUnit.SECONDS, 3)
    val MillisPerOp = TPerOp(TimeUnit.MILLISECONDS, 3)
    val MicrosPerOp = TPerOp(TimeUnit.MICROSECONDS, 3)
  }

}
