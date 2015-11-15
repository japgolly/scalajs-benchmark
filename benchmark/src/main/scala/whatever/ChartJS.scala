package whatever.chartjs

import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.CanvasRenderingContext2D
import scalajs.js
import scalajs.js.annotation._
import scalajs.js.{native, Object, |, UndefOr}
import Chart.{Labels, Value, Values}

@native
class Chart(ctx: js.Dynamic | js.Array[Canvas]) extends Object {
  def Bar(data: BarData, options: Chart.BarOptions = native): BarChart = native
}

@native
trait BarChart extends Object {
  // def getBarsAtEvent(event)
  // eachBars : function(callback){
  // buildScale : function(labels){
  // draw(ease)

  def reflow(): Unit = native
  def update(): Unit = native
  def addData(values: Values, label: String): Unit = native
  def removeData(): Unit = native
  def destroy(): Unit = native
  def scale: ChartElement1 = native
  def datasets: js.Array[DatasetB] = native
}

@native
trait ChartElement1 extends Object {
  var height: Int = native
  var width: Int = native
  var xLabels: Labels = native
  var yLabels: Labels = native
  var value: Chart.Value = native
  def calculateXLabelRotation(): Unit = native
}

@native
trait ChartElement2 extends Object {
  var value: Chart.Value = native
}

@native
trait BarData extends Object {
  var labels: Labels = native
  var datasets: js.Array[Dataset] = native
}
@native
trait DatasetB extends Dataset {
  def bars: UndefOr[js.Array[ChartElement2]] = native
}
@native
trait Dataset extends Object {
  var label: String = native
  var fillColor: String = native
  var strokeColor: String = native
  var highlightFill: String = native
  var highlightStroke: String = native
  var data: Values = native
}

@native
object Chart extends Object {
  type Value = Double
  type Values = js.Array[Value]
  type Label = String
  type Labels = js.Array[Label]

  @native
  trait GlobalOptions extends Object {

    /** Whether to animate the chart */
    var animation: Boolean = native

    /** Number of animation steps */
    var animationSteps: Int = native

    /** Animation easing effect */
    var animationEasing: String = native

    /** If we should show the scale at all */
    var showScale: Boolean = native

    /** If we want to override with a hard coded scale */
    var scaleOverride: Boolean = native

    /** Required if scaleOverride is true.
      * The number of steps in a hard coded scale */
    var scaleSteps: Double = native

    /** The value jump in the hard coded scale */
    var scaleStepWidth: Double = native

    /** The scale starting value */
    var scaleStartValue: Double = native

    /** Colour of the scale line */
    var scaleLineColor: String = native

    /** Pixel width of the scale line */
    var scaleLineWidth: Double = native

    /** Whether to show labels on the scale */
    var scaleShowLabels: Boolean = native

    /** Interpolated JS string - can access value */
    var scaleLabel: String = native

    /** Whether the scale should stick to integers, not floats even if drawing space is there */
    var scaleIntegersOnly: Boolean = native

    /** Whether the scale should start at zero, or an order of magnitude down from the lowest value */
    var scaleBeginAtZero: Boolean = native

    /** Scale label font declaration for the scale label */
    var scaleFontFamily: String = native

    /** Scale label font size in pixels */
    var scaleFontSize: Double = native

    /** Scale label font weight style */
    var scaleFontStyle: String = native

    /** Scale label font colour */
    var scaleFontColor: String = native

    /** whether or not the chart should be responsive and resize when the browser does. */
    var responsive: Boolean = native

    /** whether to maintain the starting aspect ratio or not when responsive, if set to false, will take up entire container */
    var maintainAspectRatio: Boolean = native

    /** Determines whether to draw tooltips on the canvas or not */
    var showTooltips: Boolean = native

    /** Determines whether to execute the customTooltips function instead of drawing the built in tooltips (See [Advanced - External Tooltips](#advanced-usage-custom-tooltips)) */
    var customTooltips: Boolean | js.Function1[Boolean | Tooltip, Boolean] = native

    /** Array of string names to attach tooltip events */
    var tooltipEvents: Labels = native

    /** Tooltip background colour */
    var tooltipFillColor: String = native

    /** Tooltip label font declaration for the scale label */
    var tooltipFontFamily: String = native

    /** Tooltip label font size in pixels */
    var tooltipFontSize: Double = native

    /** Tooltip font weight style */
    var tooltipFontStyle: String = native

    /** Tooltip label font colour */
    var tooltipFontColor: String = native

    /** Tooltip title font declaration for the scale label */
    var tooltipTitleFontFamily: String = native

    /** Tooltip title font size in pixels */
    var tooltipTitleFontSize: Double = native

    /** Tooltip title font weight style */
    var tooltipTitleFontStyle: String = native

    /** Tooltip title font colour */
    var tooltipTitleFontColor: String = native

    /** pixel width of padding around tooltip text */
    var tooltipYPadding: Double = native

    /** pixel width of padding around tooltip text */
    var tooltipXPadding: Double = native

    /** Size of the caret on the tooltip */
    var tooltipCaretSize: Double = native

    /** Pixel radius of the tooltip border */
    var tooltipCornerRadius: Double = native

    /** Pixel offset from point x to tooltip edge */
    var tooltipXOffset: Double = native

    /** Template string for single tooltips */
    var tooltipTemplate: String = native

    /** Template string for multiple tooltips */
    var multiTooltipTemplate: String = native

    /** Will fire on animation progression. */
    var onAnimationProgress: js.Function0[Unit] = native

    /** Will fire on animation completion. */
    var onAnimationComplete: js.Function0[Unit] = native
  }

  @native
  trait BarOptions extends GlobalOptions {
    /** Whether grid lines are shown across the chart */
    var scaleShowGridLines: Boolean = native

    /** Colour of the grid lines */
    var scaleGridLineColor: String = native

    /** Width of the grid lines */
    var scaleGridLineWidth: Double = native

    /** Whether to show horizontal lines (except X axis) */
    var scaleShowHorizontalLines: Boolean = native

    /** Whether to show vertical lines (except Y axis) */
    var scaleShowVerticalLines: Boolean = native

    /** If there is a stroke on each bar */
    var barShowStroke: Boolean = native

    /** Pixel width of the bar stroke */
    var barStrokeWidth: Double = native

    /** Spacing between each of the X value sets */
    var barValueSpacing: Double = native

    /** Spacing between data sets within X values */
    var barDatasetSpacing: Double = native

    /** A legend template */
    var legendTemplate: String = native
  }

  @native
  trait LineOptions extends GlobalOptions {
    /** Whether grid lines are shown across the chart */
    var scaleShowGridLines: Boolean = native

    /** Colour of the grid lines */
    var scaleGridLineColor: String = native

    /** Width of the grid lines */
    var scaleGridLineWidth: Double = native

    /** Whether to show horizontal lines (except X axis) */
    var scaleShowHorizontalLines: Boolean = native

    /** Whether to show vertical lines (except Y axis) */
    var scaleShowVerticalLines: Boolean = native

    /** Whether the line is curved between points */
    var bezierCurve: Boolean = native

    /** Tension of the bezier curve between points */
    var bezierCurveTension: Double = native

    /** Whether to show a dot for each point */
    var pointDot: Boolean = native

    /** Radius of each point dot in pixels */
    var pointDotRadius: Double = native

    /** Pixel width of point dot stroke */
    var pointDotStrokeWidth: Double = native

    /** amount extra to add to the radius to cater for hit detection outside the drawn point */
    var pointHitDetectionRadius: Double = native

    /** Whether to show a stroke for datasets */
    var datasetStroke: Boolean = native

    /** Pixel width of dataset stroke */
    var datasetStrokeWidth: Double = native

    /** Whether to fill the dataset with a colour */
    var datasetFill: Boolean = native

    /** A legend template */
    var legendTemplate: String = native
  }

  @native
  trait PieDoughnutOptions extends GlobalOptions {
    /** Whether we should show a stroke on each segment */
    var segmentShowStroke: Boolean = native

    /** The colour of each segment stroke */
    var segmentStrokeColor: String = native

    /** The width of each segment stroke */
    var segmentStrokeWidth: Double = native

    /** The percentage of the chart that we cut out of the middle */
    var percentageInnerCutout: Double = native

    /** Whether we animate the rotation of the Doughnut */
    var animateRotate: Boolean = native

    /** Whether we animate scaling the Doughnut from the centre */
    var animateScale: Boolean = native

    /** A legend template */
    var legendTemplate: String = native
  }


    @native
  object defaults extends Object {
    @native
    object global extends GlobalOptions
  }

  @native
  trait Tooltip extends Object {
    // todo
  }
}

/*
sealed abstract class AnimationEasing(val value: String)
object AnimationEasing {
  @native
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