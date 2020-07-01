package japgolly.scalajs.benchmark

import io.circe._
import io.circe.parser._
import japgolly.scalajs.benchmark.TestUtil._
import japgolly.scalajs.benchmark.engine._
import japgolly.scalajs.benchmark.gui.SuiteResultsFormat.{Args, JmhJson}
import japgolly.scalajs.benchmark.gui.{BMStatus, BmResultFormat, GuiOptions, GuiParam, GuiParams, GuiSuite}
import scala.concurrent.duration._
import scala.scalajs.js
import utest._

object JmhJsonTest extends TestSuite {
  override def tests = Tests {
    "simple"    - testSimple()
    "precision" - testPrecision()
    "params"    - testWithParams()
  }

  private val eo = EngineOptions.default.copy(
    warmupIterationTime = None,
    warmupIterations    = 1,
    iterations          = 4,
    iterationTime       = 2.seconds,
  )

  private val startTime =
    new js.Date()

  private def assertEqJson(actual: Json, expect: String): Unit = {
    val e = parse(expect).getOrThrow()
    assertEqJson(actual, e)
  }

  private def assertEqJson(actual: Json, expect: Json): Unit = {
    val a = actual.spaces2SortKeys
    val e = expect.spaces2SortKeys
    assertMultiline(a, e)
  }

  private def testJmhJson[P](suite     : GuiSuite[P],
                             progress  : Progress[P],
                             results   : Map[PlanKey[P], BMStatus],
                             resultFmts: Vector[BmResultFormat],
                             expect    : String): Unit = {
    val args = Args[P](Vector.empty, suite, progress, results, resultFmts, GuiOptions.default)
    val actual = JmhJson.json(args).mapArray(_.map(_.mapObject(_.filterKeys(_ != "userAgent"))))
    assertEqJson(actual, expect)
  }

  private def testJmhJsonText[P](suite     : GuiSuite[P],
                                 progress  : Progress[P],
                                 results   : Map[PlanKey[P], BMStatus],
                                 resultFmts: Vector[BmResultFormat],
                                 expect    : String): Unit = {
    val args = Args[P](Vector.empty, suite, progress, results, resultFmts, GuiOptions.default)
    val actual = JmhJson.jsonText(JmhJson.json(args).mapArray(_.map(_.mapObject(_.filterKeys(_ != "userAgent")))))
    assertMultiline(actual, expect)
  }

  private def testSimple() = {
    val bm1    = Benchmark("My BM")(())
    val suite  = Suite[Unit]("My Suite")(bm1)
    val plan   = Plan[Unit](suite, Vector.empty)
    val bm1p0  = PlanKey[Unit](0, 0)(bm1, ())
    val bm1p0r = stats(itStats(12.15), itStats(12.3, 12.2), itStats(12.3), itStats(12.1))

    val expect =
      s"""[
        |  {
        |    "benchmark" : "My_Suite.My_BM",
        |    "mode" : "avgt",
        |    "threads" : 1,
        |    "forks" : 1,
        |    "jdkVersion" : "1.8",
        |    "vmName" : "Scala.JS",
        |    "vmVersion" : "${ScalaJsInfo.version}",
        |    "warmupIterations" : 1,
        |    "warmupTime" : "2 s",
        |    "warmupBatchSize" : 1,
        |    "measurementIterations" : 4,
        |    "measurementTime" : "2 s",
        |    "measurementBatchSize" : 1,
        |    "primaryMetric" : {
        |      "score" : 12.200000000000001,
        |      "scoreError" : 0.5898962192825269,
        |      "scoreConfidence" : [
        |        11.610103780717472,
        |        12.789896219282527
        |      ],
        |      "scoreUnit" : "ms/op",
        |      "rawData" : [
        |        [
        |          12.15,
        |          12.25,
        |          12.3,
        |          12.1
        |        ]
        |      ]
        |    },
        |    "secondaryMetrics" : {
        |    }
        |  }
        |]
        |""".stripMargin.trim

    testJmhJsonText[Unit](
      suite      = GuiSuite(suite),
      progress   = Progress(startTime, plan, 123, eo),
      results    = Map(bm1p0 -> BMStatus.Done(Right(bm1p0r))),
      resultFmts = Vector(BmResultFormat.MillisPerOp),
      expect     = expect,
    )
  }

  private def testPrecision() = {
    val bm1    = Benchmark("My BM")(())
    val suite  = Suite[Unit]("My Suite")(bm1)
    val plan   = Plan[Unit](suite, Vector.empty)
    val bm1p0  = PlanKey[Unit](0, 0)(bm1, ())
    val bm1p0r = statMatrix(10, 17)((i, b) => (i+b*1.2435).micros)

    val expect =
      s"""[
        |  {
        |    "benchmark" : "My_Suite.My_BM",
        |    "mode" : "avgt",
        |    "threads" : 1,
        |    "forks" : 1,
        |    "jdkVersion" : "1.8",
        |    "vmName" : "Scala.JS",
        |    "vmVersion" : "${ScalaJsInfo.version}",
        |    "warmupIterations" : 1,
        |    "warmupTime" : "2 s",
        |    "warmupBatchSize" : 1,
        |    "measurementIterations" : 4,
        |    "measurementTime" : "2 s",
        |    "measurementBatchSize" : 1,
        |    "primaryMetric" : {
        |      "score" : 0.015691764705882353,
        |      "scoreError" : 0.0045773753094622395,
        |      "scoreConfidence" : [
        |        0.01111438939642011,
        |        0.020269140015344588
        |      ],
        |      "scoreUnit" : "ms/op",
        |      "rawData" : [
        |        [
        |          0.011191764705882352,
        |          0.012191764705882353,
        |          0.013191764705882354,
        |          0.014191764705882353,
        |          0.015191764705882356,
        |          0.016191764705882353,
        |          0.017191764705882354,
        |          0.018191764705882348,
        |          0.019191764705882353,
        |          0.020191764705882354
        |        ]
        |      ]
        |    },
        |    "secondaryMetrics" : {
        |    }
        |  }
        |]
        |""".stripMargin.trim

    testJmhJsonText[Unit](
      suite      = GuiSuite(suite),
      progress   = Progress(startTime, plan, 123, eo),
      results    = Map(bm1p0 -> BMStatus.Done(Right(bm1p0r))),
      resultFmts = Vector(BmResultFormat.MillisPerOp),
      expect     = expect,
    )
  }

  private def testWithParams() = {
    type P     = (Int, Boolean)
    val p1     = (123, true)
    val p2     = (654, false)
    val gp1    = GuiParam.int("The Size", 5, 10)
    val gp2    = GuiParam.boolean("On")
    val gps    = GuiParams.tuple2(gp1, gp2)
    val bm1    = Benchmark[P]("bm1", _ => ())
    val suite  = Suite[P]("Suite")(bm1)
    val plan   = Plan[P](suite, Vector(p1, p2))
    val bm1p1  = PlanKey[P](0, 0)(bm1, p1)
    val bm1p2  = PlanKey[P](0, 1)(bm1, p2)
    val bm1p1r = stats(itStats(.01), itStats(.009), itStats(.009), itStats(.01))
    val bm1p2r = stats(itStats(1.3), itStats(1.3), itStats(1.3), itStats(1.3))

    val expect =
      s"""[
        |  {
        |    "benchmark": "Suite.bm1",
        |    "mode": "avgt",
        |    "threads": 1,
        |    "forks": 1,
        |    "jdkVersion": "1.8",
        |    "vmName": "Scala.JS",
        |    "vmVersion": "${ScalaJsInfo.version}",
        |    "warmupIterations": 1,
        |    "warmupTime": "2 s",
        |    "warmupBatchSize": 1,
        |    "measurementIterations": 4,
        |    "measurementTime": "2 s",
        |    "measurementBatchSize": 1,
        |    "params": {
        |      "The_Size": "123",
        |      "On": "T"
        |    },
        |    "primaryMetric": {
        |      "score": 9.5,
        |      "scoreError": 3.7308312721098353,
        |      "scoreConfidence": [
        |        5.769168727890165,
        |        13.230831272109835
        |      ],
        |      "scoreUnit": "us/op",
        |      "rawData": [
        |        [
        |          10,
        |          9,
        |          9,
        |          10
        |        ]
        |      ]
        |    },
        |    "secondaryMetrics": {
        |    }
        |  },
        |  {
        |    "benchmark": "Suite.bm1",
        |    "mode": "avgt",
        |    "threads": 1,
        |    "forks": 1,
        |    "jdkVersion": "1.8",
        |    "vmName": "Scala.JS",
        |    "vmVersion": "${ScalaJsInfo.version}",
        |    "warmupIterations": 1,
        |    "warmupTime": "2 s",
        |    "warmupBatchSize": 1,
        |    "measurementIterations": 4,
        |    "measurementTime": "2 s",
        |    "measurementBatchSize": 1,
        |    "params": {
        |      "The_Size": "654",
        |      "On": "F"
        |    },
        |    "primaryMetric": {
        |      "score": 1300,
        |      "scoreError": 0,
        |      "scoreConfidence": [
        |        1300,
        |        1300
        |      ],
        |      "scoreUnit": "us/op",
        |      "rawData": [
        |        [
        |          1300,
        |          1300,
        |          1300,
        |          1300
        |        ]
        |      ]
        |    },
        |    "secondaryMetrics": {
        |    }
        |  }
        |]
        |""".stripMargin

    testJmhJson[P](
      suite      = GuiSuite(suite, gps),
      progress   = Progress(startTime, plan, 123, eo),
      results    = Map(bm1p1 -> BMStatus.Done(Right(bm1p1r)), bm1p2 -> BMStatus.Done(Right(bm1p2r))),
      resultFmts = Vector(BmResultFormat.MicrosPerOp, BmResultFormat.OpsPerSec),
      expect     = expect,
    )
  }
}
