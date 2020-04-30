package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.engine._
import japgolly.scalajs.benchmark.gui.Styles.{Suite => *}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._

/** Format for a number of results.
  */
abstract class FormatResults(final val label: String) {
  def render[P](args: FormatResults.Args[P]): VdomElement
}

object FormatResults {

  val builtIn: Vector[FormatResults] =
    Vector(Table, Text)

  final case class Args[P](suite     : GuiSuite[P],
                           progress  : Progress[P],
                           results   : Map[PlanKey[P], BMStatus],
                           resultFmts: Vector[FormatResult]) {
    val resultFmtCount = resultFmts.length
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

            case BMDone(Right(r)) =>
              runsCell(r.runs) +:
                resultFmts.flatMap(f => Vector(
                  resultTD(f.score render r),
                  plusMinusCell,
                  resultTD(f.error render r)))
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

  case object Text extends FormatResults("Text") {
    override def render[P](args: Args[P]): VdomElement = {
      import args._

      val keys = progress.plan.keys

      val rowBuilder = List.newBuilder[Vector[String]]

      def header: Vector[String] =
        ("Benchmark" +: suite.params.headers :+ "Runs") ++ resultFmts.iterator.flatMap(f => f.header :: "±" :: "error" :: Nil)

      rowBuilder += header
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
          case BMDone(Right(r)) =>
            cells :+= Util.addThousandSeps(FormatValue.Integer toText r.runs)
            for (f <- resultFmts)
              cells ++= Vector(
                Util.addThousandSeps(f.score toText r),
                "±",
                Util.addThousandSeps(f.error toText r))
        }

        rowBuilder += cells
      }

      val preResultColumns = suite.params.headers.length + 1

      def gap(i: Int): String =
        if (i <= preResultColumns || ((i - preResultColumns) % 3) == 0)
          "     "
        else
          " "

      val rows = rowBuilder.result()
      val text = Util.formatTable(rows, gap)
      <.pre(*.resultText, text)
    }
  }

}
