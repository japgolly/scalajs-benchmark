package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import Styles.{BatchMode => *}
import monocle.Lens
import monocle.macros.Lenses

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

    final case class Initial(items: Vector[Item[Unit, Unit]]) extends State

    @Lenses
    final case class Running(batchPlans: BatchPlans, items: Vector[Item[SuiteState, Unit]]) extends State

    case object Finished extends State

    type SuiteState = Option[SuiteRunner.State[_]]

    def init(p: Props): State =
      Initial(Item.fromTocItems(p.items))
  }

  sealed trait BatchPlan {
    val guiPlan: GuiPlan
    val start  : AsyncCallback[SuiteRunner.RunCtrls[guiPlan.Param]]
  }

  object BatchPlan {
    def apply(p: GuiPlan)(s: AsyncCallback[SuiteRunner.RunCtrls[p.Param]]): BatchPlan =
      new BatchPlan {
        override val guiPlan: p.type = p
        override val start = s
      }
  }

  final case class BatchPlans(plans: Vector[BatchPlan], runnableItems: Vector[Item[State.SuiteState, Unit]]) {

    val newState =
      State.Running(this, runnableItems)

    def isEmpty =
      totalBMs == 0

    val totalBMs: Int =
      plans.iterator.map(_.guiPlan.totalBMs).sum

    def etaMs(o: EngineOptions): Double =
      o.estimatedMsPerBM * totalBMs

    def startA($: StateAccessPure[State]): AsyncCallback[Unit] =
      $.setStateAsync(newState) >> plans.foldLeft(AsyncCallback.unit)((q, p) => q >> p.start.void)

    def start($: StateAccessPure[State]): Callback =
      Callback.byName(startA($).toCallback)
  }

  private implicit val reusabilityStateRunnerState: Reusability[SuiteRunner.State[_]] = Reusability.byRef
  implicit val reusabilityBatchPlan: Reusability[BatchPlan] = Reusability.byRef
  implicit val reusabilityBatchPlans: Reusability[BatchPlans] = Reusability.byRef
  implicit val reusabilityStateI: Reusability[State.Initial] = Reusability.derive
  implicit val reusabilityStateR: Reusability[State.Running] = Reusability.derive
  implicit val reusabilityStateF: Reusability[State.Finished.type] = Reusability.derive
  implicit val reusabilityState: Reusability[State] = Reusability.derive

  // ===================================================================================================================

  final class Backend($: BackendScope[Props, State]) {
    import State.SuiteState

    private val initialSetState =
      StateSnapshot.withReuse.prepare[Vector[Item[Unit, Unit]]]((os, cb) => $.setStateOption(os.map(State.Initial), cb))

    private val initialRenderItem: Item[Unit, Unit] ~=> VdomNode =
      Reusable.byRef(_.name)

    private val initialRenderBM: BatchModeTree.RenderBM[Unit, Unit] ~=> VdomNode =
      Reusable.byRef(_.name)

    private val runningRenderItem: Item[SuiteState, Unit] ~=> VdomNode =
      Reusable.byRef {
        case i: Item.Folder[SuiteState, Unit] => i.name
        case i: Item.Suite [SuiteState, Unit] =>
          i.value match {
            case Some(ss) =>
              ss.status match {
                case SuiteRunner.SuitePending       => i.name
                case SuiteRunner.SuiteWillStart     => i.name + " (starting)"
                case _: SuiteRunner.SuiteRunning[_] => i.name + " (running)"
                case _: SuiteRunner.SuiteDone[_]    => i.name + " (done)"
              }
            case None => i.name
          }
      }
    // TODO ↕ do this properly later ↕
    private val runningRenderBM: BatchModeTree.RenderBM[SuiteState, Unit] ~=> VdomNode =
      Reusable.byRef { i =>
        i.suiteItem.value match {
          case Some(ss) =>
            ss.status match {
              case SuiteRunner.SuitePending       => i.name
              case SuiteRunner.SuiteWillStart     => i.name
              case s: SuiteRunner.SuiteRunning[_] =>
                val status =
                  s.bm.iterator.filter(_._1.bmIndex == i.idx).map(_._2)
                  .foldLeft[BMStatus](BMStatus.Pending)(_ merge _)
                status match {
                  case BMStatus.Pending   => i.name
                  case BMStatus.Preparing => i.name + " (starting)"
                  case BMStatus.Running   => i.name + " (running)"
                  case _: BMStatus.Done   => i.name + " (done)"
                }
              case _: SuiteRunner.SuiteDone[_] => i.name + " (done)"
            }
          case None => i.name
        }
      }

    private val unsafeStateRunningLens: Lens[State, State.Running] =
      GuiUtil.unsafeNarrowLens

    private val unsafeStateRunningItemsLens =
      unsafeStateRunningLens ^|-> State.Running.items

    private def planBatches(initialState : State.Initial,
                            engineOptions: EngineOptions,
                            guiOptions   : GuiOptions): BatchPlans = {
      type Item1 = Item[Unit, Unit]
      type Item2 = Item[SuiteState, Unit]
      type Suite1 = Item.Suite[Unit, Unit]
      type Suite2 = Item.Suite[SuiteState, Unit]

      val plans = Vector.newBuilder[BatchPlan]

      def runStateLens[P]: Lens[Suite2, SuiteRunner.State[P]] =
        Lens((_: Suite2).value.get.asInstanceOf[SuiteRunner.State[P]])(a => _.copy(value = Some(a)))

      def planSuite[P](i: Suite1, guiSuite: GuiSuite[P], $$: StateAccessPure[Suite2]): Suite2 = {
        import i.{bms => bmItems}
        var suiteState: SuiteState = None
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
            val guiPlan = GuiPlan(guiSuite2)(params)
            val stateAccess = $$.zoomStateL(runStateLens[P])
            val run = SuiteRunner.run(guiPlan)(stateAccess, engineOptions)
            val batchPlan = BatchPlan(guiPlan)(run)
            plans += batchPlan
            val initialRunState = SuiteRunner.State.init(guiSuite2, guiOptions, respectDisabledByDefault = false)
            suiteState = Some(initialRunState)
          }
        }
        i.copy(value = suiteState)
      }

      val folderItems: Lens[Item2, Vector[Item2]] =
        GuiUtil.unsafeNarrowLens[Item2, Item.Folder[SuiteState, Unit]] ^|-> Item.Folder.children

      val castSuiteL: Lens[Item2, Suite2] =
        GuiUtil.unsafeNarrowLens

      def loop(in: Vector[Item1], $$: StateAccessPure[Vector[Item2]]): Vector[Item2] =
        in.indices.iterator.map { idx =>
          val $$$ = $$.zoomStateL(GuiUtil.vectorIndex(idx))
          in(idx) match {
            case i: Item.Folder[Unit, Unit] => i.copy(children = loop(i.children, $$$.zoomStateL(folderItems)))
            case i: Item.Suite [Unit, Unit] => planSuite(i, i.suite, $$$.zoomStateL(castSuiteL))
          }
        }.toVector

      val item2s = loop(initialState.items, $.zoomStateL(unsafeStateRunningItemsLens))
      BatchPlans(plans.result(), item2s)
    }

    private def renderStatus(p: Props, batchPlans: BatchPlans): VdomNode = {
      def kv(key: VdomNode, value: VdomNode) =
        <.tr(<.td(key), <.td(value))

      val eta = batchPlans.etaMs(p.engineOptions)

      <.table(
        <.tbody(
          kv("Benchmarks", batchPlans.totalBMs),
          kv("ETA", GuiUtil.formatETA(eta)),
        ))
    }

    private def renderInitial(p: Props, s: State.Initial): VdomNode = {
      val batchPlans = planBatches(s, p.engineOptions, p.guiOptions)

      val tree = BatchModeTree.Args(
        state      = initialSetState(s.items),
        renderItem = initialRenderItem,
        renderBM   = initialRenderBM,
        enabled    = Enabled,
      )

      val start =
        <.button(
          ^.disabled := batchPlans.isEmpty,
          ^.onClick --> batchPlans.start($),
          "Start")

      <.div(
        <.section(renderStatus(p, batchPlans), start),
        <.section(tree.render),
      )
    }

    private def renderRunning(p: Props, s: State.Running): VdomNode = {
      val tree = BatchModeTree.Args(
        state      = StateSnapshot.withReuse(s.items).readOnly,
        renderItem = runningRenderItem,
        renderBM   = runningRenderBM,
        enabled    = Disabled,
      )

      <.div(
        <.section(renderStatus(p, s.batchPlans)),
        <.section(tree.render),
      )
    }

    def render(p: Props, s: State): VdomNode =
      s match {
        case s: State.Initial => renderInitial(p, s)
        case s: State.Running => renderRunning(p, s)
        case State.Finished => "?"
      }
  }

  val Component = ScalaComponent.builder[Props]
    .initialStateFromProps(State.init)
    .renderBackend[Backend]
    .build
}