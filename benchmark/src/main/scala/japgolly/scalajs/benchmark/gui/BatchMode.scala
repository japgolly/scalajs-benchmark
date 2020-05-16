package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.Benchmark
import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.components.TriStateCheckbox
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import Styles.{BatchMode => *}
import japgolly.scalajs.react.extra.StateSnapshot

object BatchMode {
  import BatchModeTree.Item

  val name = "Batch Mode"

  final case class Props(items        : Seq[TableOfContents.Item.NonBatchMode],
                         engineOptions: EngineOptions,
                         guiOptions   : GuiOptions) {
    @inline def render: VdomElement = Component(this)
  }

  final case class BatchPlan(guiPlans: Vector[GuiPlan]) {
    val totalBMs: Int =
      guiPlans.iterator.map(_.totalBMs).sum

    def etaMs(o: EngineOptions): Double =
      o.estimatedMsPerBM * totalBMs

  }


  sealed trait State
  object State {
    final case class Initial(items: Vector[Item[Unit]]) extends State
    case object Running extends State
    case object Finished extends State

    def init(p: Props): State =
      Initial(Item.fromTocItems(p.items))

    implicit val reusabilityI: Reusability[Initial] = Reusability.derive
    implicit val reusabilityR: Reusability[Running.type] = Reusability.derive
    implicit val reusabilityF: Reusability[Finished.type] = Reusability.derive
    implicit val reusability: Reusability[State] = Reusability.derive
  }

  final class Backend($: BackendScope[Props, State]) {

    private val setStateInitial =
      StateSnapshot.withReuse.prepare[Vector[Item[Unit]]]((os, cb) => $.setStateOption(os.map(State.Initial), cb))

    private def planBatch(p: Props, s: State.Initial): BatchPlan = {
      val plans = Vector.newBuilder[GuiPlan]

//      def go(items: Iterable[Item.NonBatchMode], suiteIdPrefix: String): Unit =
//        for ((item, idx) <- items.iterator.zipWithIndex) {
//          val suiteId = if (suiteIdPrefix.isEmpty) idx.toString else suiteIdPrefix + "." + idx
//          item match {
//            case folder: Item.Folder =>
//              go(folder.children, suiteId)
//            case i: Item.Suite =>
//              def process[P](guiSuite: GuiSuite[P]): Unit = {
//                val bms = guiSuite.suite.bms
//                val keys = bms.indices.iterator.map(BMKey(suiteId, _)).filterNot(s.disabledBMs.contains)
//                if (keys.hasNext) {
//                  val selectedBMs = keys.map(k => bms(k.bmIdx)).toVector
//                  val guiSuite2   = guiSuite.withBMs(selectedBMs)
//                  // plans += GuiPlan(guiSuite2)(params)
//                }
//              }
//              process(i.suite)
//          }
//        }

      BatchPlan(plans.result())
    }

    private def renderStatus(p: Props, s: State.Initial): VdomNode = {
      def kv(key: VdomNode, value: VdomNode) =
        <.tr(<.td(key), <.td(value))

      val batchPlan = planBatch(p, s)
      val eta = batchPlan.etaMs(p.engineOptions)

      <.table(
        <.tbody(
          kv("Benchmarks", batchPlan.totalBMs),
          kv("ETA", GuiUtil.formatETA(eta)),
        ))
    }


    private def renderInitial(p: Props, s: State.Initial): VdomNode = {
      <.div(
        <.section(renderStatus(p, s)),
        <.section(BatchModeTree.Args(setStateInitial(s.items), Enabled).render),
      )
    }

    def render(p: Props, s: State): VdomNode =
      s match {
        case s: State.Initial => renderInitial(p, s)
        case State.Running
           | State.Finished => "?"
      }
  }

  val Component = ScalaComponent.builder[Props]
    .initialStateFromProps(State.init)
    .renderBackend[Backend]
    .build
}