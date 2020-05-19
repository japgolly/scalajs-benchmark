package japgolly.scalajs.benchmark.gui

import io.circe._
import io.circe.syntax._
import japgolly.microlibs.stdlib_ext.MutableArray
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
abstract class SuiteResultsFormat(final val label: String) {
  def render[P](args: SuiteResultsFormat.Args[P]): VdomElement
}

object SuiteResultsFormat {

  val builtIn: Vector[SuiteResultsFormat] =
    Vector(Table, JmhText, JmhJson, CSV(8))

  val builtInBatch: Map[SuiteResultsFormat.Text, Enabled] =
    Map(
      JmhJson -> Enabled,
      JmhText -> Enabled,
      CSV(8)  -> Disabled,
    )

  final case class Args[P](suite     : GuiSuite[P],
                           progress  : Progress[P],
                           results   : Map[PlanKey[P], BMStatus],
                           resultFmts: Vector[BmResultFormat],
                           guiOptions: GuiOptions) {

    val resultFmtCount = resultFmts.length

    def filename(ext: String): String = {
      val name = guiOptions.resultFilenameWithoutExt(suite.suite, progress)
      s"$name.$ext"
    }
  }

  // ===================================================================================================================

  /** A format that generates text. */
  abstract class Text(label: String,
                      final val mimeType: String,
                      final val fileExt: String) extends SuiteResultsFormat(label) {

    def renderToText[P](args: Args[P]): String

    override def render[P](args: Args[P]): VdomElement =
      TextOutput.Props(
        text     = renderToText(args),
        mimeType = mimeType,
        filename = args.filename(fileExt),
      ).render

    def save[P](args: Args[P]): Callback =
      GuiUtil.saveFile(
        text     = renderToText(args),
        filename = args.filename(fileExt),
        mimeType = mimeType,
      )
  }

  implicit val reusabilityText: Reusability[Text] =
    Reusability.byRef

  // ===================================================================================================================

  case object Table extends SuiteResultsFormat("Table") {
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
        resultTD(ValueFormat.Integer render runs)

      def rows =
        keys.map { k =>
          val status = results.getOrElse(k, BMStatus.Pending)
          var hs = Vector.empty[VdomTag]
          hs :+= resultTD(k.bm.name)
          hs ++= suite.params.renderParams(k.param).map(resultTD(_))
          hs ++= (status match {
            case BMStatus.Pending        => whenBMPending
            case BMStatus.Preparing      => whenBMPreparing
            case BMStatus.Running        => whenBMRunning

            case BMStatus.Done(Left(err)) =>
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

            case BMStatus.Done(Right(stats)) =>
              runsCell(stats.samples) +:
                resultFmts.flatMap(f => Vector(
                  resultTD(f.score render stats),
                  plusMinusCell,
                  resultTD(f.scoreError render stats)))
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
                   prettyNumbers      : Boolean,
                  ): Vector[Vector[String]] = {
    import args._

    val decFmt =
      overridePrecision.map { dp => "%." + dp + "f" }

    def formatNum[A](valueFormat: ValueFormat[A], value: A): String =
      decFmt match {
        case Some(fmt) =>
          GuiUtil.removeTrailingZeros(fmt.format(valueFormat.toDouble(value)))
        case None =>
          if (prettyNumbers)
            valueFormat.toTextPretty(value)
          else
            valueFormat.toTextBasic(value)
      }

    val rowBuilder = Vector.newBuilder[Vector[String]]

    def header: Vector[String] =
      ("Benchmark" +: suite.params.headers :+ "Cnt") ++ resultFmts.iterator.flatMap(f =>
        if (separatePlusMinus)
          f.header :: "±" :: "error" :: Nil
        else
          f.header :: "± error" :: Nil)

    rowBuilder += header

    if (emptyRowAfterHeader)
      rowBuilder += Vector.empty

    val keys = MutableArray(progress.plan.keys).sortBy(k => (k.bmIndex, k.paramIndex))
    for (k <- keys.iterator) {
      var cells = Vector.empty[String]
      val status = results.getOrElse(k, BMStatus.Pending)

      cells :+= k.bm.name
      cells ++= suite.params.renderParamsToText(k.param)

      status match {
        case BMStatus.Pending        => ()
        case BMStatus.Preparing      => cells :+= "Preparing..."
        case BMStatus.Running        => cells :+= "Running..."
        case BMStatus.Done(Left(e))  => cells :+= ("" + e).takeWhile(_ != '\n')
        case BMStatus.Done(Right(s)) =>
          cells :+= formatNum(ValueFormat.Integer, s.samples)
          for (f <- resultFmts) {
            val score = formatNum(f.score, s)
            val error = formatNum(f.scoreError, s)
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

  case object JmhText extends Text("JMH Text", "text/plain", "txt") {

    override def renderToText[P](args: Args[P]): String = {
      import args._

      val rows = textTable(
        args                = args,
        separatePlusMinus   = true,
        emptyRowAfterHeader = true,
        overridePrecision   = None,
        prettyNumbers       = true,
      )

      val preResultColumns = suite.params.headers.length + 1

      def gap(i: Int): String =
        if (i <= preResultColumns || ((i - preResultColumns) % 3) == 0)
          "     "
        else
          " "

      GuiUtil.formatTable(rows, gap)
    }
  }

  // ===================================================================================================================

  case object JmhJson extends Text("JMH JSON", "application/json", "json") {

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
          val fmtRes        = resultFmts.head
          val hasParams     = suite.params.editors.nonEmpty
          val engineOptions = progress.engineOptions
          results.iterator.collect {
            case (key, BMStatus.Done(Right(stats))) =>

              val params: Option[Map[String, String]] =
                if (hasParams)
                  Some {
                    suite.params.renderParamsToText(key.param).iterator.zipWithIndex.map { case (paramValue, idx) =>
                      val name = suite.params.headers(idx).replace(' ', '_') // jmh-visualizer doesn't like spaces in param names
                      name -> paramValue
                    }.toMap
                  }
                else
                  None

              val scoreConfidence1 = fmtRes.scoreConfidence1.toDouble(stats)
              val scoreConfidence2 = fmtRes.scoreConfidence2.toDouble(stats)

              val primaryMetric =
                PrimaryMetric(
                  score           = fmtRes.score.toDouble(stats),
                  scoreError      = fmtRes.scoreError.toDouble(stats),
                  scoreConfidence = Vector(scoreConfidence1, scoreConfidence2),
                  scoreUnit       = fmtRes.header.replace('μ', 'u'),
                  rawData         = Vector(stats.isolatedBatches.map(b => fmtRes.score.toDouble(b))),
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
                warmupIterations      = engineOptions.warmupIterations,
                warmupTime            = durToStr(engineOptions.actualWarmupIterationTime),
                warmupBatchSize       = 1,
                measurementIterations = engineOptions.iterations,
                measurementTime       = durToStr(engineOptions.iterationTime),
                measurementBatchSize  = 1,
                params                = params,
                primaryMetric         = primaryMetric,
                secondaryMetrics      = SecondaryMetrics(),
              )
          }.toVector
        }

      benchmarksJson.asJson.deepDropNullValues
    }

    def jsonText(json: Json): String =
      json
        .spaces2
        .replaceAll("\n +\n", "\n") // Remove blank lines

    override def renderToText[P](args: Args[P]): String =
      jsonText(json(args))
  }

  // ===================================================================================================================

  final case class CSV(decimalPoints: Int) extends Text("CSV", "text/csv", "csv") {
    override def renderToText[P](args: Args[P]): String = {
      val rows = textTable(
        args                = args,
        separatePlusMinus   = false,
        emptyRowAfterHeader = false,
        overridePrecision   = Some(decimalPoints),
        prettyNumbers       = false,
      )
      GuiUtil.formatCSV(rows)
    }
  }

}
