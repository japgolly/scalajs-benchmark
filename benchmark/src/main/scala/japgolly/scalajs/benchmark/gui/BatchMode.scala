package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import Styles.{BatchMode => *}
import japgolly.scalajs.benchmark.Benchmark
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
    final case class Running(batchPlans: BatchPlans, items: Vector[Item[SuiteState, Int]]) extends State

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

  final case class BatchPlans(plans: Vector[BatchPlan], runnableItems: Vector[Item[State.SuiteState, Int]]) {

    val newState =
      State.Running(this, runnableItems)

    def isEmpty =
      totalBMs == 0

    val totalBMs: Int =
      plans.iterator.map(_.guiPlan.totalBMs).sum

    def etaMs(o: EngineOptions): Double =
      o.estimatedMsPerBM * totalBMs

    def startA($: StateAccessPure[State]): AsyncCallback[Unit] =
      $.setStateAsync(newState) >> plans.foldLeft(AsyncCallback.unit)((q, p) => q >> p.start.flatMap(_.onCompletion).void)

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

    private lazy val statusOfItem: Item[SuiteState, Int] => *.Status = {
      case i: Item.Folder[SuiteState, Int] =>
        if (i.children.isEmpty)
          *.Status.Disabled
        else
          i.children.iterator.map(statusOfItem).reduce(_ merge _)

      case i: Item.Suite [SuiteState, Int] =>
        i.value match {
          case Some(ss) =>
            ss.status match {
              case SuiteRunner.SuitePending       => *.Status.Pending
              case SuiteRunner.SuiteWillStart     => *.Status.Preparing
              case _: SuiteRunner.SuiteRunning[_] => *.Status.Running
              case _: SuiteRunner.SuiteDone[_]    => *.Status.Done
            }
          case None => *.Status.Disabled
        }
    }

    private val runningRenderItem: Item[SuiteState, Int] ~=> VdomNode =
      Reusable.byRef { item =>
        val status = statusOfItem(item)
        <.div(*.runningItem(status), item.name)
      }

    private val bmStatusToStyleStatus: BMStatus => *.Status = {
      case BMStatus.Pending   => *.Status.Pending
      case BMStatus.Preparing => *.Status.Preparing
      case BMStatus.Running   => *.Status.Running
      case _: BMStatus.Done   => *.Status.Done
    }

    private val runningRenderBM: BatchModeTree.RenderBM[SuiteState, Int] ~=> VdomNode =
      Reusable.byRef { i =>
        var suffix = EmptyVdom
        val status: *.Status =
          i.bmItem.enabled match {
            case Enabled =>
              i.suiteItem.value match {
                case Some(ss) =>
                  ss.status match {
                    case SuiteRunner.SuitePending       => *.Status.Pending
                    case SuiteRunner.SuiteWillStart     => *.Status.Preparing
                    case _: SuiteRunner.SuiteDone[_]    => *.Status.Done
                    case s: SuiteRunner.SuiteRunning[_] =>
                      val bmIdx = i.bmItem.value
                      val statuses =
                        s.bm
                          .iterator
                          .filter(_._1.bmIndex == bmIdx)
                          .map(_._2)
                          .map(bmStatusToStyleStatus)
                          .toVector
                      var status = statuses.foldLeft[*.Status](*.Status.Done)(_ merge _)
                      val runs   = statuses.length
                      val params = s.plan.params.length
                      if (runs < params && status == *.Status.Done)
                        // This BM is Done with previous params but there are more to come
                        status = if (runs == 0) *.Status.Pending else *.Status.Incomplete
                      if (runs > 0 && params > 1)
                        suffix = s" ($runs/$params)"
                      status
                  }
                case None => *.Status.Disabled
              }
            case Disabled =>
              *.Status.Disabled
          }
        <.div(*.runningItem(status), i.name, suffix)
      }

    private val unsafeStateRunningLens: Lens[State, State.Running] =
      GuiUtil.unsafeNarrowLens

    private val unsafeStateRunningItemsLens =
      unsafeStateRunningLens ^|-> State.Running.items

    private def planBatches(initialState : State.Initial,
                            engineOptions: EngineOptions,
                            guiOptions   : GuiOptions): BatchPlans = {
      type Item1 = Item[Unit, Unit]
      type Item2 = Item[SuiteState, Int]
      type Suite1 = Item.Suite[Unit, Unit]
      type Suite2 = Item.Suite[SuiteState, Int]

      val plans = Vector.newBuilder[BatchPlan]

      def runStateLens[P]: Lens[Suite2, SuiteRunner.State[P]] =
        Lens((_: Suite2).value.get.asInstanceOf[SuiteRunner.State[P]])(a => _.copy(value = Some(a)))

      def planSuite[P](i: Suite1, guiSuite: GuiSuite[P], $$: StateAccessPure[Suite2]): Suite2 = {
        import i.{bms => bmItems}
        var result: Suite2 = null
        for (params <- guiSuite.defaultParams) {
          var enabledBMs = Vector.empty[Benchmark[P]]
          val newBmItems =
            bmItems.iterator.zipWithIndex.map { case (bmItem, idx) =>
              if (bmItem.enabled is Enabled) {
                enabledBMs :+= guiSuite.suite.bms(idx)
                bmItem.copy(value = enabledBMs.length - 1)
              } else
                bmItem.copy(value = -1)
            }.toVector
          if (enabledBMs.nonEmpty) {
            val guiSuite2 = guiSuite.withBMs(enabledBMs)
            val guiPlan = GuiPlan(guiSuite2)(params)
            val stateAccess = $$.zoomStateL(runStateLens[P])
            val run = SuiteRunner.run(guiPlan)(stateAccess, engineOptions)
            val batchPlan = BatchPlan(guiPlan)(run)
            plans += batchPlan
            val initialRunState = SuiteRunner.State.init(guiSuite2, guiOptions, respectDisabledByDefault = false)
            result = i.copy(value = Some(initialRunState), bms = newBmItems)
          }
        }
        if (result eq null)
          i.copy(value = None, bms = i.bms.map(_.copy(value = -1)))
        else
          result
      }

      val folderItems: Lens[Item2, Vector[Item2]] =
        GuiUtil.unsafeNarrowLens[Item2, Item.Folder[SuiteState, Int]] ^|-> Item.Folder.children

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
        state          = initialSetState(s.items),
        renderItem     = initialRenderItem,
        renderBM       = initialRenderBM,
        enabled        = Enabled,
        showCheckboxes = true,
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
        state          = StateSnapshot.withReuse(s.items).readOnly,
        renderItem     = runningRenderItem,
        renderBM       = runningRenderBM,
        enabled        = Disabled,
        showCheckboxes = false,
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