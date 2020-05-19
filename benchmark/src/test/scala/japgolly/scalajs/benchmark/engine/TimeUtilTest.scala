package japgolly.scalajs.benchmark.engine

import japgolly.microlibs.testutil.TestUtil._
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{FiniteDuration, TimeUnit}
import sourcecode.Line
import utest._

object TimeUtilTest extends TestSuite {

  override def tests = Tests {
    "getUnitsFromMs" - {
      def test(t: TimeUnit)(implicit l: Line): Unit = {
        val oneAsMs = TimeUtil.toMs(FiniteDuration(1, t))
        assertEq(TimeUtil.getUnitsFromMs(t)(oneAsMs), 1)
      }

      "NANOSECONDS"  - test(TimeUnit.NANOSECONDS)
      "MICROSECONDS" - test(TimeUnit.MICROSECONDS)
      "MILLISECONDS" - test(TimeUnit.MILLISECONDS)
      "SECONDS"      - test(TimeUnit.SECONDS)
      "MINUTES"      - test(TimeUnit.MINUTES)
      "HOURS"        - test(TimeUnit.HOURS)
      "DAYS"         - test(TimeUnit.DAYS)
    }
  }
}
