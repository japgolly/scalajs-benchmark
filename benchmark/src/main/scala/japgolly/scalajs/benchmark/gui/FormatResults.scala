package japgolly.scalajs.benchmark.gui

import io.circe._
import io.circe.syntax._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.engine._
import japgolly.scalajs.benchmark.gui.Styles.{Suite => *}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import java.util.concurrent.TimeUnit
import org.scalajs.dom.window
import scala.concurrent.duration.FiniteDuration
import scalacss.ScalaCssReact._

/** Format for a number of results.
  */
abstract class FormatResults(final val label: String) {
  def render[P](args: FormatResults.Args[P]): VdomElement
}

object FormatResults {

  val builtIn: Vector[FormatResults] =
    Vector(Table, Text, CSV(8), JmhJson)

  final case class Args[P](suite     : GuiSuite[P],
                           progress  : Progress[P],
                           results   : Map[PlanKey[P], BMStatus],
                           resultFmts: Vector[FormatResult]) {

    val resultFmtCount = resultFmts.length

    def filename(ext: String): String =
      s"sjsbm-${suite.suite.filenameFriendlyName}-${progress.timestampTxt}.$ext"
  }

  // ===================================================================================================================

  case object Table extends FormatResults("Table") {
    private val resultBlock1  = ^.colSpan := 3
    private val resultTD      = <.td(*.resultData)
    private val plusMinusCell = resultTD("±")
    private val runsCellNone  = resultTD

    override def render[P](args: Args[P]): VdomElement = {
      import args._

      val keys            = progress.plan.keys
      val resultBlockAll  = ^.colSpan := (3 * resultFmtCount)
      val whenBMPending   = Vector[VdomTag](runsCellNone, resultTD(resultBlockAll))
      val whenBMPreparing = Vector[VdomTag](runsCellNone, resultTD(resultBlockAll, *.preparing, "Preparing…"))
      val whenBMRunning   = Vector[VdomTag](runsCellNone, resultTD(resultBlockAll, *.running, "Running…"))

      def header = {
        val th = <.th(*.resultHeader)
        var hs = Vector.empty[VdomTag]
        hs :+= th("Benchmark")
        hs ++= suite.params.headers.map(th(_))
        hs :+= th("Runs")
        hs ++= resultFmts.map(f => <.th(*.resultHeaderScore, resultBlock1, f.header))
        <.tr(hs: _*)
      }

      def runsCell(runs: Int) =
        resultTD(FormatValue.Integer render runs)

      def rows =
        keys.map { k =>
          val status = results.getOrElse(k, BMPending)
          var hs = Vector.empty[VdomTag]
          hs :+= resultTD(k.bm.name)
          hs ++= suite.params.renderParams(k.param).map(resultTD(_))
          hs ++= (status match {
            case BMPending        => whenBMPending
            case BMPreparing      => whenBMPreparing
            case BMRunning        => whenBMRunning

            case BMDone(Left(err)) =>
              val showError = Callback {
                err.printStackTrace()
              }
              Vector[VdomTag](
                runsCellNone, // Hmmmmm.........
                resultTD(
                  resultBlockAll,
                  <.span(^.color.red, Option(err.toString).filter(_.nonEmpty).getOrElse[String]("ERROR.")),
                  ^.title := "Double-click to print the error to the console",
                  ^.cursor.pointer,
                  ^.onDoubleClick --> showError))

            case BMDone(Right(stats)) =>
              runsCell(stats.samples) +:
                resultFmts.flatMap(f => Vector(
                  resultTD(f.score render stats.score),
                  plusMinusCell,
                  resultTD(f.scoreError render stats.scoreError)))
          })
          <.tr(hs: _*)
        }

      <.div(
        <.table(
          *.resultTable,
          <.thead(header),
          <.tbody(rows: _*)))
    }
  }

  // ===================================================================================================================

  def textTable[P](args               : Args[P],
                   separatePlusMinus  : Boolean,
                   emptyRowAfterHeader: Boolean,
                   overridePrecision  : Option[Int],
                   modNumber          : String => String,
                  ): Vector[Vector[String]] = {
    import args._

    val decFmt =
      overridePrecision.map { dp => "%." + dp + "f" }

    def formatNum[A](formatValue: FormatValue[A], value: A): String = {
      val str =
        decFmt match {
          case Some(fmt) => TextUtil.removeTrailingZeros(fmt.format(formatValue.toDouble(value)))
          case None      => formatValue.toText(value)
        }
      modNumber(str)
    }

    val keys = progress.plan.keys

    val rowBuilder = Vector.newBuilder[Vector[String]]

    def header: Vector[String] =
      ("Benchmark" +: suite.params.headers :+ "Runs") ++ resultFmts.iterator.flatMap(f =>
        if (separatePlusMinus)
          f.header :: "±" :: "error" :: Nil
        else
          f.header :: "± error" :: Nil)

    rowBuilder += header

    if (emptyRowAfterHeader)
      rowBuilder += Vector.empty

    for (k <- keys) {
      var cells = Vector.empty[String]
      val status = results.getOrElse(k, BMPending)

      cells :+= k.bm.name
      cells ++= suite.params.renderParamsToText(k.param)

      status match {
        case BMPending        => ()
        case BMPreparing      => cells :+= "Preparing..."
        case BMRunning        => cells :+= "Running..."
        case BMDone(Left(e))  => cells :+= ("" + e).takeWhile(_ != '\n')
        case BMDone(Right(s)) =>
          cells :+= formatNum(FormatValue.Integer, s.samples)
          for (f <- resultFmts) {
            val score = formatNum(f.score, s.score)
            val error = formatNum(f.scoreError, s.scoreError)
            val c =
              if (separatePlusMinus)
                Vector(score, "±", error)
              else
                Vector(score, error)
            cells ++= c
          }
      }

      rowBuilder += cells
    }

    rowBuilder.result()
  }

  // ===================================================================================================================

  case object Text extends FormatResults("Text") {
    override def render[P](args: Args[P]): VdomElement = {
      import args._

      val rows = textTable(
        args                = args,
        separatePlusMinus   = true,
        emptyRowAfterHeader = true,
        overridePrecision   = None,
        modNumber           = TextUtil.addThousandSeps
      )

      val preResultColumns = suite.params.headers.length + 1

      def gap(i: Int): String =
        if (i <= preResultColumns || ((i - preResultColumns) % 3) == 0)
          "     "
        else
          " "

      val text = TextUtil.formatTable(rows, gap)

      TextOutput.Props(
        text = text,
        mimeType = "text/plain",
        filename = args.filename("txt"),
      ).render
    }
  }

  // ===================================================================================================================

  final case class CSV(decimalPoints: Int) extends FormatResults("CSV") {
    override def render[P](args: Args[P]): VdomElement = {
      val rows = textTable(
        args                = args,
        separatePlusMinus   = false,
        emptyRowAfterHeader = false,
        overridePrecision   = Some(decimalPoints),
        modNumber           = identity
      )
      val text = TextUtil.formatCSV(rows)
      TextOutput.Props(
        text = text,
        mimeType = "text/csv",
        filename = args.filename("csv"),
      ).render
    }
  }


  // ===================================================================================================================

  case object JmhJson extends FormatResults("JMH JSON") {

    private object Internals {
      type BenchmarksJson = Vector[BenchmarkJson]

      // "jmhVersion": "1.21",
      // "jvm": "/usr/lib/jvm/java-11-graalvm/bin/java",
      // "jvmArgs": [ ... ]
      final case class BenchmarkJson(benchmark            : String,
                                     mode                 : String,
                                     threads              : Int,
                                     forks                : Int,
                                     jdkVersion           : String,
                                     vmName               : String,
                                     vmVersion            : String,
                                     userAgent            : String, // custom
                                     warmupIterations     : Int,
                                     warmupTime           : String,
                                     warmupBatchSize      : Int,
                                     measurementIterations: Int,
                                     measurementTime      : String,
                                     measurementBatchSize : Int,
                                     params               : Option[Map[String, String]],
                                     primaryMetric        : PrimaryMetric,
                                     secondaryMetrics     : SecondaryMetrics)

      final case class PrimaryMetric(score          : Double,
                                     scoreError     : Double,
                                     scoreConfidence: Vector[Double],
                                     scoreUnit      : String,
                                     rawData        : Vector[Vector[Double]]) // outer is per-fork

      final case class SecondaryMetrics()
    }

    def json[P](args: Args[P]): Json = {
      import io.circe.generic.auto._
      import Internals._
      import args._

      def durToStr(fd: FiniteDuration): String = {
        val u = fd.unit match {
          case TimeUnit.NANOSECONDS  => "ns"
          case TimeUnit.MICROSECONDS => "us"
          case TimeUnit.MILLISECONDS => "ms"
          case TimeUnit.SECONDS      => "s"
          case TimeUnit.MINUTES      => "m"
          case TimeUnit.HOURS        => "h"
          case TimeUnit.DAYS         => "d"
        }
        s"${fd.length} $u"
      }

      val benchmarksJson: BenchmarksJson =
        if (resultFmts.isEmpty)
          Vector.empty
        else {
          val fmtRes = resultFmts.head
          val hasParams = suite.params.editors.nonEmpty
          results.iterator.collect {
            case (key, BMDone(Right(stats))) =>

              val params: Option[Map[String, String]] =
                if (hasParams)
                  Some {
                    suite.params.renderParamsToText(key.param).iterator.zipWithIndex.map { case (param, idx) =>
                      suite.params.headers(idx) -> param
                    }.toMap
                  }
                else
                  None

              val primaryMetric =
                PrimaryMetric(
                  score           = fmtRes.score.toDouble(stats.score),
                  scoreError      = fmtRes.scoreError.toDouble(stats.scoreError),
                  scoreConfidence = Vector(stats.scoreConfidence._1, stats.scoreConfidence._2).map(fmtRes.score.toDouble),
                  scoreUnit       = fmtRes.header.replace('μ', 'u'),
                  rawData         = Vector(stats.isolatedBatches.map(b => fmtRes.score.toDouble(b.score))),
                )

              BenchmarkJson(
                benchmark             = s"${suite.suite.name}.${key.bm.name}".replace(' ', '_'),
                mode                  = "avgt",
                threads               = 1,
                forks                 = 1,
                jdkVersion            = System.getProperty("java.version", "?"),
                vmName                = "Scala.JS",
                vmVersion             = ScalaJsInfo.version,
                userAgent             = window.navigator.userAgent,
                warmupIterations      = stats.engineOptions.warmupIterations,
                warmupTime            = durToStr(stats.engineOptions.actualWarmupIterationTime),
                warmupBatchSize       = 1,
                measurementIterations = stats.engineOptions.iterations,
                measurementTime       = durToStr(stats.engineOptions.iterationTime),
                measurementBatchSize  = 1,
                params                = params,
                primaryMetric         = primaryMetric,
                secondaryMetrics      = SecondaryMetrics(),
              )
          }.toVector
        }

      benchmarksJson.asJson.deepDropNullValues
    }

    override def render[P](args: Args[P]): VdomElement = {
      val text = json(args).spaces2
      TextOutput.Props(
        text = text,
        mimeType = "application/json",
        filename = args.filename("json"),
      ).render
    }
  }

}
