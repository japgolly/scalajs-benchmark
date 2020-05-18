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

    "OpsPerSec" - {
      val fmt = BmResultFormat.OpsPerSec
      "score" - {
        val f = fmt.score
        assertEq(f.toTextBasic(sample5), "235")
        assertEq(f.toTextBasic(sample3), "5")
        assertEq(f.toTextBasic(sample4), "50")
      }
      "error" - {
        val f = fmt.scoreError
        assertEq(f.toTextBasic(sample5), "74")
        assertEq(f.toTextBasic(sample3), "5")
        assertEq(f.toTextBasic(sample4), "46")
      }
    }
  }
}
