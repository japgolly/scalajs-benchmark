package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.vendor.chartjs._
import japgolly.scalajs.react._
import japgolly.scalajs.react.internal.JsUtil
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html.Canvas
import scalajs.js

object ReactChart {
  private def newObj[T <: js.Object]: T =
    js.Object().asInstanceOf[T]

  final case class ScalaBarData(
    labels  : Vector[String],
    datasets: Vector[ScalaDataset]) {

    def toJs: BarData = {
      val d      = newObj[BarData]
      d.labels   = js.Array(labels: _*)
      d.datasets = js.Array(datasets.map(_.toJs): _*)
      d
    }
  }

  final case class ScalaDataset(
    label          : String,
    data           : Vector[Chart.Value],
    fillColor      : js.UndefOr[String] = js.undefined,
    strokeColor    : js.UndefOr[String] = js.undefined,
    highlightFill  : js.UndefOr[String] = js.undefined,
    highlightStroke: js.UndefOr[String] = js.undefined) {

    def toJs: Dataset = {
      val d   = newObj[Dataset]
      d.label = label
      d.data  = js.Array(data: _*)
      fillColor      .foreach(d.fillColor       = _)
      strokeColor    .foreach(d.strokeColor     = _)
      highlightFill  .foreach(d.highlightFill   = _)
      highlightStroke.foreach(d.highlightStroke = _)
      d
    }
  }

  sealed trait Effect

  final case class InfoForFx(min: Double, max: Double, value: Double, datasetIndex: Int) {
    val range = max - min
    val pct: Double =
      if (range <= 0)
        1.0
      else
        (value - min) / range
  }

  type InfoToColour = InfoForFx => String

  final case class ColourByValue(stroke: Option[InfoToColour] = None, fill: Option[InfoToColour] = None) extends Effect

  object ColourByValue {
    def scaleFn(from: RGB, to: RGB): InfoToColour = {
      val dr = (to.r - from.r).toDouble
      val dg = (to.g - from.g).toDouble
      val db = (to.b - from.b).toDouble
      def fix(d: Double): Int =
        d.toInt min 255 max 0
      x => {
        val r = fix(x.pct * dr + from.r)
        val g = fix(x.pct * dg + from.g)
        val b = fix(x.pct * db + from.b)
        s"rgb($r,$g,$b)"
      }
    }
  }

  final case class RGB(r: Int, g: Int, b: Int)

  final case class Props(style: TagMod,
                         data: ScalaBarData,
                         options: Chart.BarOptions = newObj,
                         fx: Option[Effect] = None) {
    @inline def render: VdomElement = Component(this)
  }

  type State = Option[BarChart]

  final class Backend($: BackendScope[Props, State]) {
    def render(p: Props) =
      <.canvas(p.style)

    private def newChart(p: Props): CallbackTo[BarChart] =
      $.getDOMNode.map { dom =>
        val canvas = dom.asMounted().domCast[Canvas]
        val c = new Chart(canvas.getContext("2d")).Bar(p.data.toJs, p.options)
        // js.Dynamic.global.ccc = c
        c
      }

    def mount: Callback =
      $.props >>= newChart >>= (c => $ setState Some(c))

    val update: Callback =
      for {
        c <- $.state.asCBO[BarChart]
        p <- $.props.toCBO
      } yield {

        def xs = c.scale.xLabels.length

        while (xs > p.data.labels.length)
          c.removeData()

        for (i <- xs until p.data.labels.length) {
          val values = new js.Array[Chart.Value]()
          p.data.datasets.take(xs).foreach(values push _.data(i))
          c.addData(values, p.data.labels(i))
        }

        for {
          (d, i) <- p.data.datasets.iterator.zipWithIndex
          (v, j) <- d.data.iterator.zipWithIndex
        } {
          c.datasets.lift(i).flatMap(_.bars.toOption).flatMap(_.lift(j)) match {
            case Some(e) => e.value = v
            case None    =>
          }
        }

        p.fx.foreach(applyFx(c, _, p.data).runNow())

        c.scale.xLabels = JsUtil.jsArrayFromTraversable(p.data.labels)
        c.scale.calculateXLabelRotation()
        c.update()
      }

    def applyFx(c: BarChart, fx: Effect, data: ScalaBarData) = Callback {
      fx match {
        case x: ColourByValue =>
          // ignore errors and the -0.1 while a BM is running
          val ignoreValue: Double => Boolean = _ < 0

          for ((ds, i) <- data.datasets.iterator.zipWithIndex) {
            var min = Double.MaxValue
            var max = Double.MinValue
            for (v <- ds.data if !ignoreValue(v)) {
              if (v < min) min = v
              if (v > max) max = v
            }
            for ((v, j) <- ds.data.iterator.zipWithIndex if !ignoreValue(v)) {
              val r = InfoForFx(min, max, v, i)
              val m = c.datasets(i).bars.get(j)
              x.fill.foreach(f => m.fillColor = f(r))
              x.stroke.foreach(f => m.strokeColor = f(r))
            }
          }
      }
    }

    def unmount: Callback =
      for {
        c <- $.state.asCBO[BarChart]
        _ <- Callback(c.destroy())
        _ <- $.setState(None)
      } yield ()
  }

  val Component = ScalaComponent.builder[Props]
    .initialState[State](None)
    .renderBackend[Backend]
    .componentDidMount(_.backend.mount)
    .componentWillUnmount(_.backend.unmount)
    .componentDidUpdate(_.backend.update)
    .build
}
