package japgolly.scalajs.benchmark.vendor.chartjs

import japgolly.scalajs.benchmark.vendor.chartjs.Chart.{Labels, Values}
import org.scalajs.dom.html.Canvas
import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.|

@JSGlobal("Chart")
@js.native
@nowarn("cat=unused")
class Chart(ctx: js.Dynamic | js.Array[Canvas]) extends js.Object {
  def Bar(data: BarData, options: Chart.BarOptions = js.native): BarChart = js.native
}

@js.native
@nowarn("cat=unused")
trait BarChart extends js.Object {
  // def getBarsAtEvent(event)
  // eachBars : function(callback){
  // buildScale : function(labels){
  // draw(ease)

  def reflow(): Unit = js.native
  def update(): Unit = js.native
  def addData(values: Values, label: String): Unit = js.native
  def removeData(): Unit = js.native
  def destroy(): Unit = js.native
  def scale: ChartElement1 = js.native
  def datasets: js.Array[DatasetB] = js.native
}

@js.native
@nowarn("cat=unused")
trait ChartElement1 extends js.Object {
  var height: Int = js.native
  var width: Int = js.native
  var xLabels: Labels = js.native
  var yLabels: Labels = js.native
  var value: Chart.Value = js.native
  def calculateXLabelRotation(): Unit = js.native
}

@js.native
@nowarn("cat=unused")
trait ChartElement2 extends js.Object {
  var value: Chart.Value = js.native
  var fillColor: String = js.native
  var strokeColor: String = js.native
  var highlightFill: String = js.native
  var highlightStroke: String = js.native
}

@js.native
@nowarn("cat=unused")
trait BarData extends js.Object {
  var labels: Labels = js.native
  var datasets: js.Array[Dataset] = js.native
}

@js.native
@nowarn("cat=unused")
trait DatasetB extends Dataset {
  def bars: js.UndefOr[js.Array[ChartElement2]] = js.native
}

@js.native
@nowarn("cat=unused")
trait Dataset extends js.Object {
  var label: String = js.native
  var fillColor: String = js.native
  var strokeColor: String = js.native
  var highlightFill: String = js.native
  var highlightStroke: String = js.native
  var data: Values = js.native
}

@JSGlobal("Chart")
@js.native
@nowarn("cat=unused")
object Chart extends js.Object {
  type Value = Double
  type Values = js.Array[Value]
  type Label = String
  type Labels = js.Array[Label]

  @js.native
  @nowarn("cat=unused")
  trait GlobalOptions extends js.Object {

    /** Whether to animate the chart */
    var animation: Boolean = js.native

    /** Number of animation steps */
    var animationSteps: Int = js.native

    /** Animation easing effect */
    var animationEasing: String = js.native

    /** If we should show the scale at all */
    var showScale: Boolean = js.native

    /** If we want to override with a hard coded scale */
    var scaleOverride: Boolean = js.native

    /** Required if scaleOverride is true.
      * The number of steps in a hard coded scale */
    var scaleSteps: Double = js.native

    /** The value jump in the hard coded scale */
    var scaleStepWidth: Double = js.native

    /** The scale starting value */
    var scaleStartValue: Double = js.native

    /** Colour of the scale line */
    var scaleLineColor: String = js.native

    /** Pixel width of the scale line */
    var scaleLineWidth: Double = js.native

    /** Whether to show labels on the scale */
    var scaleShowLabels: Boolean = js.native

    /** Interpolated JS string - can access value */
    var scaleLabel: String = js.native

    /** Whether the scale should stick to integers, not floats even if drawing space is there */
    var scaleIntegersOnly: Boolean = js.native

    /** Whether the scale should start at zero, or an order of magnitude down from the lowest value */
    var scaleBeginAtZero: Boolean = js.native

    /** Scale label font declaration for the scale label */
    var scaleFontFamily: String = js.native

    /** Scale label font size in pixels */
    var scaleFontSize: Double = js.native

    /** Scale label font weight style */
    var scaleFontStyle: String = js.native

    /** Scale label font colour */
    var scaleFontColor: String = js.native

    /** whether or not the chart should be responsive and resize when the browser does. */
    var responsive: Boolean = js.native

    /** whether to maintain the starting aspect ratio or not when responsive, if set to false, will take up entire container */
    var maintainAspectRatio: Boolean = js.native

    /** Determines whether to draw tooltips on the canvas or not */
    var showTooltips: Boolean = js.native

    /** Determines whether to execute the customTooltips function instead of drawing the built in tooltips (See [Advanced - External Tooltips](#advanced-usage-custom-tooltips)) */
    var customTooltips: Boolean | js.Function1[Boolean | Tooltip, Boolean] = js.native

    /** Array of string names to attach tooltip events */
    var tooltipEvents: Labels = js.native

    /** Tooltip background colour */
    var tooltipFillColor: String = js.native

    /** Tooltip label font declaration for the scale label */
    var tooltipFontFamily: String = js.native

    /** Tooltip label font size in pixels */
    var tooltipFontSize: Double = js.native

    /** Tooltip font weight style */
    var tooltipFontStyle: String = js.native

    /** Tooltip label font colour */
    var tooltipFontColor: String = js.native

    /** Tooltip title font declaration for the scale label */
    var tooltipTitleFontFamily: String = js.native

    /** Tooltip title font size in pixels */
    var tooltipTitleFontSize: Double = js.native

    /** Tooltip title font weight style */
    var tooltipTitleFontStyle: String = js.native

    /** Tooltip title font colour */
    var tooltipTitleFontColor: String = js.native

    /** pixel width of padding around tooltip text */
    var tooltipYPadding: Double = js.native

    /** pixel width of padding around tooltip text */
    var tooltipXPadding: Double = js.native

    /** Size of the caret on the tooltip */
    var tooltipCaretSize: Double = js.native

    /** Pixel radius of the tooltip border */
    var tooltipCornerRadius: Double = js.native

    /** Pixel offset from point x to tooltip edge */
    var tooltipXOffset: Double = js.native

    /** Template string for single tooltips */
    var tooltipTemplate: String = js.native

    /** Template string for multiple tooltips */
    var multiTooltipTemplate: String = js.native

    /** Will fire on animation progression. */
    var onAnimationProgress: js.Function0[Unit] = js.native

    /** Will fire on animation completion. */
    var onAnimationComplete: js.Function0[Unit] = js.native
  }

  @js.native
  @nowarn("cat=unused")
  trait BarOptions extends GlobalOptions {
    /** Whether grid lines are shown across the chart */
    var scaleShowGridLines: Boolean = js.native

    /** Colour of the grid lines */
    var scaleGridLineColor: String = js.native

    /** Width of the grid lines */
    var scaleGridLineWidth: Double = js.native

    /** Whether to show horizontal lines (except X axis) */
    var scaleShowHorizontalLines: Boolean = js.native

    /** Whether to show vertical lines (except Y axis) */
    var scaleShowVerticalLines: Boolean = js.native

    /** If there is a stroke on each bar */
    var barShowStroke: Boolean = js.native

    /** Pixel width of the bar stroke */
    var barStrokeWidth: Double = js.native

    /** Spacing between each of the X value sets */
    var barValueSpacing: Double = js.native

    /** Spacing between data sets within X values */
    var barDatasetSpacing: Double = js.native

    /** A legend template */
    var legendTemplate: String = js.native
  }

  @js.native
  @nowarn("cat=unused")
  trait LineOptions extends GlobalOptions {
    /** Whether grid lines are shown across the chart */
    var scaleShowGridLines: Boolean = js.native

    /** Colour of the grid lines */
    var scaleGridLineColor: String = js.native

    /** Width of the grid lines */
    var scaleGridLineWidth: Double = js.native

    /** Whether to show horizontal lines (except X axis) */
    var scaleShowHorizontalLines: Boolean = js.native

    /** Whether to show vertical lines (except Y axis) */
    var scaleShowVerticalLines: Boolean = js.native

    /** Whether the line is curved between points */
    var bezierCurve: Boolean = js.native

    /** Tension of the bezier curve between points */
    var bezierCurveTension: Double = js.native

    /** Whether to show a dot for each point */
    var pointDot: Boolean = js.native

    /** Radius of each point dot in pixels */
    var pointDotRadius: Double = js.native

    /** Pixel width of point dot stroke */
    var pointDotStrokeWidth: Double = js.native

    /** amount extra to add to the radius to cater for hit detection outside the drawn point */
    var pointHitDetectionRadius: Double = js.native

    /** Whether to show a stroke for datasets */
    var datasetStroke: Boolean = js.native

    /** Pixel width of dataset stroke */
    var datasetStrokeWidth: Double = js.native

    /** Whether to fill the dataset with a colour */
    var datasetFill: Boolean = js.native

    /** A legend template */
    var legendTemplate: String = js.native
  }

  @js.native
  @nowarn("cat=unused")
  trait PieDoughnutOptions extends GlobalOptions {
    /** Whether we should show a stroke on each segment */
    var segmentShowStroke: Boolean = js.native

    /** The colour of each segment stroke */
    var segmentStrokeColor: String = js.native

    /** The width of each segment stroke */
    var segmentStrokeWidth: Double = js.native

    /** The percentage of the chart that we cut out of the middle */
    var percentageInnerCutout: Double = js.native

    /** Whether we animate the rotation of the Doughnut */
    var animateRotate: Boolean = js.native

    /** Whether we animate scaling the Doughnut from the centre */
    var animateScale: Boolean = js.native

    /** A legend template */
    var legendTemplate: String = js.native
  }


  @js.native
  object defaults extends js.Object {

    @js.native
    object global extends GlobalOptions
  }

  @js.native
  trait Tooltip extends js.Object {
    // todo
  }
}

/*
sealed abstract class AnimationEasing(val value: String)
object AnimationEasing {
  @js.native
  trait Str extends js.Any
  implicit def

  case object EaseInOutQuart   extends AnimationEasing("easeInOutQuart")
  case object Linear           extends AnimationEasing("linear")
  case object EaseOutBounce    extends AnimationEasing("easeOutBounce")
  case object EaseInBack       extends AnimationEasing("easeInBack")
  case object EaseInOutQuad    extends AnimationEasing("easeInOutQuad")
  case object EaseOutQuart     extends AnimationEasing("easeOutQuart")
  case object EaseOutQuad      extends AnimationEasing("easeOutQuad")
  case object EaseInOutBounce  extends AnimationEasing("easeInOutBounce")
  case object EaseOutSine      extends AnimationEasing("easeOutSine")
  case object EaseInOutCubic   extends AnimationEasing("easeInOutCubic")
  case object EaseInExpo       extends AnimationEasing("easeInExpo")
  case object EaseInOutBack    extends AnimationEasing("easeInOutBack")
  case object EaseInCirc       extends AnimationEasing("easeInCirc")
  case object EaseInOutElastic extends AnimationEasing("easeInOutElastic")
  case object EaseOutBack      extends AnimationEasing("easeOutBack")
  case object EaseInQuad       extends AnimationEasing("easeInQuad")
  case object EaseInOutExpo    extends AnimationEasing("easeInOutExpo")
  case object EaseInQuart      extends AnimationEasing("easeInQuart")
  case object EaseOutQuint     extends AnimationEasing("easeOutQuint")
  case object EaseInOutCirc    extends AnimationEasing("easeInOutCirc")
  case object EaseInSine       extends AnimationEasing("easeInSine")
  case object EaseOutExpo      extends AnimationEasing("easeOutExpo")
  case object EaseOutCirc      extends AnimationEasing("easeOutCirc")
  case object EaseOutCubic     extends AnimationEasing("easeOutCubic")
  case object EaseInQuint      extends AnimationEasing("easeInQuint")
  case object EaseInElastic    extends AnimationEasing("easeInElastic")
  case object EaseInOutSine    extends AnimationEasing("easeInOutSine")
  case object EaseInOutQuint   extends AnimationEasing("easeInOutQuint")
  case object EaseInBounce     extends AnimationEasing("easeInBounce")
  case object EaseOutElastic   extends AnimationEasing("easeOutElastic")
  case object EaseInCubic      extends AnimationEasing("easeInCubic")
}
*/