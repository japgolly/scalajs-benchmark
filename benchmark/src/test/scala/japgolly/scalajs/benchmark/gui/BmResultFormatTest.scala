package japgolly.scalajs.benchmark.gui

import scala.concurrent.duration._
import japgolly.scalajs.benchmark.TestUtil._
import java.util.concurrent.TimeUnit
import scalaz.Equal
import utest._

object BmResultFormatTest extends TestSuite {
  import BmResultFormat._

  private implicit val equalBmResultFormat: Equal[BmResultFormat] = Equal.equalRef[BmResultFormat]

  private val sample1 = statPlusMinus(123456123.nanos, 10.millis)
  private val sample2 = statPlusMinus(4.537.millis, 8.65.micros)
  private val sample3 = statPlusMinus(200.millis, 10.millis)
  private val sample4 = statPlusMinus(20.millis, 1.millis)
  private val sample5 = stats(230.073, 236.728, 237.375).modifyMeans(1000 / _) // 234.726 ± 73.747  ops/s
  private val sample6 = stats(15.423, 15.323, 15.395, 15.296, 15.234, 15.391, 15.61, 15.291, 15.665).modifyMeans(_ / 1000)
  private val sample7 = statMatrix(10, 100)((_, _) => 90995.nanos)

  override def tests = Tests {

    "MillisPerOp" - {
      val fmt = MillisPerOp
      "score" - {
        val f = fmt.score
        assertEq(f.toTextBasic(sample1), "123.456")
        assertEq(f.toTextBasic(sample2), "4.537")
        assertEq(f.toTextBasic(sample3), "200.000")
        assertEq(f.toTextBasic(sample4), "20.000")
        assertEq(f.toTextBasic(sample5), "4.261")
        assertEq(f.toDouble(sample5), 4.261149)
      }
      "error" - {
        val f = fmt.scoreError
        assertEq(f.toTextBasic(sample1), "182.437")
        assertEq(f.toTextBasic(sample2), "0.158")
        assertEq(f.toTextBasic(sample3), "182.437")
        assertEq(f.toTextBasic(sample4), "18.244")
        assertEq(f.toTextBasic(sample5), "1.352")
        assertEq(f.toDouble(sample5), 1.351744)
      }
      "scoreConfidence" - {
        assertEqWithTolerance(fmt.scoreConfidence1.toDouble(sample5), 4.261149 - 1.351744, 0.00001)
        assertEqWithTolerance(fmt.scoreConfidence2.toDouble(sample5), 4.261149 + 1.351744, 0.00001)
      }
    }

    "MicrosPerOp" - {
      val fmt = MicrosPerOp
      "score" - {
        val f = fmt.score
        assertEq(f.toTextBasic(sample6), "15.403")
        assertEq(f.toTextBasic(sample7), "90.995")
      }
      "error" - {
        val f = fmt.scoreError
        assertEq(f.toTextBasic(sample6), "0.245")
      }
    }

    "OpsPerSec" - {
      val fmt = OpsPerSec
      "score" - {
        val f = fmt.score
        assertEq(f.toTextBasic(sample5), "235")
        assertEq(f.toTextBasic(sample3), "5")
        assertEq(f.toTextBasic(sample4), "50")
        assertEq(f.toTextBasic(sample6), "64927")
        assertEq(f.toTextBasic(sample7), "10990")
        assertEq(f.toDouble(sample7), 10989.614814000759)
      }
      "error" - {
        val f = fmt.scoreError
        assertEq(f.toTextBasic(sample5), "74")
        assertEq(f.toTextBasic(sample3), "5")
        assertEq(f.toTextBasic(sample4), "46")
        assertEq(f.toTextBasic(sample6), "1029")
      }
    }

    "OpsPerSec3" - {
      val fmt = OpsPerSec3
      "score" - {
        val f = fmt.score
        assertEq(f.toTextBasic(sample6), "64927.114")
      }
      "error" - {
        val f = fmt.scoreError
        assertEq(f.toTextBasic(sample6), "1028.505")
        assertEq(f.toDouble(sample6), 1028.5050681366954)
      }
    }

    "OpsPerMs" - {
      val fmt = opsPerTime(TimeUnit.MILLISECONDS, 3, 3)
      "score" - {
        val f = fmt.score
        assertEq(f.toTextBasic(sample6), "64.927")
        assertEq(f.toTextBasic(sample7), "10.990")
        assertEq(f.toDouble(sample6), 64.92711395552942)
        assertEq(f.toDouble(sample7), 10.98961481400076)
      }
      "error" - {
        val f = fmt.scoreError
        assertEq(f.toDouble(sample6), 1.0285050681366972)
      }
      "scoreConfidence" - {
        assertEqWithTolerance(fmt.scoreConfidence1.toDouble(sample6), 64.92711395552942 - 1.0285050681366972, 0.00001)
        assertEqWithTolerance(fmt.scoreConfidence2.toDouble(sample6), 64.92711395552942 + 1.0285050681366972, 0.00001)
      }
    }

    "chooseTimePerOp" - {
      "NaN"    - assertEq(chooseTimePerOp(Duration.Undefined), MicrosPerOp)
      "ns"     - assertEq(chooseTimePerOp(123.nanos)         , MicrosPerOp)
      "us"     - assertEq(chooseTimePerOp(123.micros)        , MicrosPerOp)
      "ms"     - assertEq(chooseTimePerOp(123.millis)        , MillisPerOp)
      "sec"    - assertEq(chooseTimePerOp(1.23.seconds)      , SecPerOp3)
      "sec10"  - assertEq(chooseTimePerOp(12.3.seconds)      , SecPerOp2)
      "sec100" - assertEq(chooseTimePerOp(123.seconds)       , SecPerOp2)
    }
  }
}
