package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import Styles.{BatchMode => *}

object BatchMode {
  import BatchModeTree.Item

  val name = "Batch Mode"

  final case class Props(items        : Seq[TableOfContents.Item.NonBatchMode],
                         engineOptions: EngineOptions,
                         guiOptions   : GuiOptions) {
    @inline def render: VdomElement = Component(this)
  }

  // ===================================================================================================================

  sealed trait State
  object State {
    final case class Initial(items: Vector[Item[Unit]]) extends State
    final case class Running(items: Vector[Item[Unit]]) extends State
    case object Finished extends State

    def init(p: Props): State =
      Initial(Item.fromTocItems(p.items))

    implicit val reusabilityI: Reusability[Initial] = Reusability.derive
    implicit val reusabilityR: Reusability[Running] = Reusability.derive
    implicit val reusabilityF: Reusability[Finished.type] = Reusability.derive
    implicit val reusability: Reusability[State] = Reusability.derive
  }

  // ===================================================================================================================

  final case class BatchPlan(guiPlans: Vector[GuiPlan]) {
    def isEmpty =
      totalBMs == 0

    val totalBMs: Int =
      guiPlans.iterator.map(_.totalBMs).sum

    def etaMs(o: EngineOptions): Double =
      o.estimatedMsPerBM * totalBMs
  }

  object BatchPlan {
    def from(s: State.Initial): BatchPlan = {
      val plans = Vector.newBuilder[GuiPlan]

      def process[P](guiSuite: GuiSuite[P], bmItems: Vector[Item.BM[Unit]]): Unit =
        for (params <- guiSuite.defaultParams) {
          val bms =
            bmItems
              .indices
              .iterator
              .filter(bmItems(_).enabled is Enabled)
              .map(guiSuite.suite.bms)
              .toVector
          if (bms.nonEmpty) {
            val guiSuite2 = guiSuite.withBMs(bms)
            plans += GuiPlan(guiSuite2)(params)
          }
        }

      def go(items: Vector[Item[Unit]]): Unit =
        items.foreach {
          case i: Item.Suite [Unit] => process(i.suite, i.bms)
          case i: Item.Folder[Unit] => go(i.children)
        }

      go(s.items)

      BatchPlan(plans.result())
    }
  }

  // ===================================================================================================================

  final class Backend($: BackendScope[Props, State]) {

    private val initialSetState =
      StateSnapshot.withReuse.prepare[Vector[Item[Unit]]]((os, cb) => $.setStateOption(os.map(State.Initial), cb))

    private val initialRenderItem: Item[Unit] ~=> VdomNode =
      Reusable.byRef(_.name)

    private val initialRenderBM: BatchModeTree.RenderBM[Unit] ~=> VdomNode =
      Reusable.byRef(_.name)


    private def renderStatus(p: Props, batchPlan: BatchPlan): VdomNode = {
      def kv(key: VdomNode, value: VdomNode) =
        <.tr(<.td(key), <.td(value))

      val eta = batchPlan.etaMs(p.engineOptions)

      <.table(
        <.tbody(
          kv("Benchmarks", batchPlan.totalBMs),
          kv("ETA", GuiUtil.formatETA(eta)),
        ))
    }

    private def renderInitial(p: Props, s: State.Initial): VdomNode = {
      val batchPlan = BatchPlan.from(s)
      val tree = BatchModeTree.Args(
        state      = initialSetState(s.items),
        renderItem = initialRenderItem,
        renderBM   = initialRenderBM,
        enabled    = Enabled,
      )

      if (!batchPlan.isEmpty) {
        batchPlan
          .guiPlans
          .map(guiPlan => {
            val s = SuiteRunner.State.init(guiPlan.guiSuite, p.guiOptions, respectDisabledByDefault = false)
            // ↑ store above in state
            // ↓ $.zoomState
            SuiteRunner.run(guiPlan)(ss, p.engineOptions)
          })
      }


      <.div(
        <.section(renderStatus(p, batchPlan)),
        <.section(tree.render),
      )
    }

    def render(p: Props, s: State): VdomNode =
      s match {
        case s: State.Initial => renderInitial(p, s)
        case _: State.Running
           | State.Finished => "?"
      }
  }

  val Component = ScalaComponent.builder[Props]
    .initialStateFromProps(State.init)
    .renderBackend[Backend]
    .build
}