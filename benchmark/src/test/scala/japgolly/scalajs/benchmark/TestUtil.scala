package japgolly.scalajs.benchmark

import japgolly.scalajs.benchmark.engine._
import scala.concurrent.duration._

object TestUtil extends japgolly.microlibs.testutil.TestUtil {

  implicit def durationToMs(d: Duration): Double =
    TimeUtil.toMs(d)

  implicit def msToDuration(d: Double): Duration =
    TimeUtil.fromMs(d)

  def stats(durs: Duration*): Stats =
    Stats(durs.iterator.map(IterationStats(1, _)).toVector)

  def statPlusMinus(dur: FiniteDuration, pm: FiniteDuration) =
    stats(dur, dur + pm, dur - pm)

}
