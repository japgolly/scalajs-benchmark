package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark.TestUtil._
import utest._

object StatMathTest extends TestSuite {

  override def tests = Tests {
    "T-Dist" - {
      val confidence = 0.999
      val p = 1 - (1 - confidence) / 2
      def t(df: Int) = StatMath.tDistributionInverseCumulativeProbability(df, p)
      assertEqWithTolerance(t(1), 636.619248768754)
      assertEqWithTolerance(t(2), 31.59905457644449)
      assertEqWithTolerance(t(3), 12.923978636691256)
      assertEqWithTolerance(t(4), 8.610301581379362)
      assertEqWithTolerance(t(5), 6.868826625877448)
      assertEqWithTolerance(t(6), 5.958816178395137)
      assertEqWithTolerance(t(7), 5.40788252068221)
      assertEqWithTolerance(t(8), 5.041305433373389)
      assertEqWithTolerance(t(9), 4.780912585931209)
      assertEqWithTolerance(t(10), 4.586893858705167)
      assertEqWithTolerance(t(11), 4.436979338234592)
      assertEqWithTolerance(t(12), 4.31779128360619)
      assertEqWithTolerance(t(13), 4.220831727707025)
      assertEqWithTolerance(t(14), 4.140454112642893)
      assertEqWithTolerance(t(15), 4.072765196361243)
      assertEqWithTolerance(t(16), 4.014996327184215)
      assertEqWithTolerance(t(17), 3.9651262721193383)
      assertEqWithTolerance(t(18), 3.9216458250857205)
      assertEqWithTolerance(t(19), 3.883405852593182)
      assertEqWithTolerance(t(20), 3.8495162749325607)
      assertEqWithTolerance(t(21), 3.81927716427684)
      assertEqWithTolerance(t(22), 3.7921306717013312)
      assertEqWithTolerance(t(23), 3.7676268043150865)
      assertEqWithTolerance(t(24), 3.7453986192936024)
      assertEqWithTolerance(t(25), 3.725143949728728)
      assertEqWithTolerance(t(26), 3.706611743480941)
      assertEqWithTolerance(t(27), 3.689591713459319)
      assertEqWithTolerance(t(28), 3.6739064007013247)
      assertEqWithTolerance(t(29), 3.6594050194663867)
      assertEqWithTolerance(t(30), 3.6459586350420787)
    }

    "sameAsJmh" - {
      val s = stats(1337.911, 1748.364, 1332.656, 1316.026, 1380.979, 1307.462, 1302.26, 1306.425, 1309.876)
      assertEqWithTolerance(s.score, 1371.328777)
      assertEqWithTolerance(s.scoreError, 241.156949)
      assertEqWithTolerance(s.scoreConfidence._1, 1130.171828)
      assertEqWithTolerance(s.scoreConfidence._2, 1612.485726)
    }
  }
}
