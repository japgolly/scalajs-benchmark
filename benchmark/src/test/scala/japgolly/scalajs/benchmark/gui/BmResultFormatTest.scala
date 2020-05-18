package japgolly.scalajs.benchmark.gui

import scala.concurrent.duration._
import japgolly.scalajs.benchmark.TestUtil._
import utest._

object BmResultFormatTest extends TestSuite {

  private val sample1 = statPlusMinus(123456123.nanos, 10.millis)
  private val sample2 = statPlusMinus(4.537.millis, 8.65.micros)
  private val sample3 = statPlusMinus(200.millis, 10.millis)
  private val sample4 = statPlusMinus(20.millis, 1.millis)
  private val sample5 = stats(230.073, 236.728, 237.375).map(1000 / _) // 234.726 ± 73.747  ops/s
  private val sample6 = stats(15.423, 15.323, 15.395, 15.296, 15.234, 15.391, 15.61, 15.291, 15.665).map(_ / 1000)

  override def tests = Tests {

    "MillisPerOp" - {
      val fmt = BmResultFormat.MillisPerOp
      "score" - {
        val f = fmt.score
        assertEq(f.toTextBasic(sample1), "123.456")
        assertEq(f.toTextBasic(sample2), "4.537")
        assertEq(f.toTextBasic(sample3), "200.000")
        assertEq(f.toTextBasic(sample4), "20.000")
        assertEq(f.toTextBasic(sample5), "4.261")
      }
      "error" - {
        val f = fmt.scoreError
        assertEq(f.toTextBasic(sample1), "182.437")
        assertEq(f.toTextBasic(sample2), "0.158")
        assertEq(f.toTextBasic(sample3), "182.437")
        assertEq(f.toTextBasic(sample4), "18.244")
        assertEq(f.toTextBasic(sample5), "1.352")
      }
    }

    "MicrosPerOp" - {
      val fmt = BmResultFormat.MicrosPerOp
      "score" - {
        val f = fmt.score
        assertEq(f.toTextBasic(sample6), "15.403")
      }
      "error" - {
        val f = fmt.scoreError
        assertEq(f.toTextBasic(sample6), "0.245")
      }
    }

    "OpsPerSec" - {
      val fmt = BmResultFormat.OpsPerSec
      "score" - {
        val f = fmt.score
        assertEq(f.toTextBasic(sample5), "235")
        assertEq(f.toTextBasic(sample3), "5")
        assertEq(f.toTextBasic(sample4), "50")
        assertEq(f.toTextBasic(sample6), "64927")
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
      val fmt = BmResultFormat.OpsPerSec3
      "score" - {
        val f = fmt.score
        assertEq(f.toTextBasic(sample6), "64927.114")
      }
      "error" - {
        val f = fmt.scoreError
        assertEq(f.toTextBasic(sample6), "1028.505")
      }
    }
  }
}
