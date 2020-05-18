package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.Benchmark
import japgolly.scalajs.benchmark.engine.{AbortFn, EngineOptions}
import japgolly.scalajs.benchmark.gui.BatchMode.State.RunningStatus
import japgolly.scalajs.benchmark.gui.Styles.{BatchMode => *}
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.{GenPrism, Lenses}
import monocle.{Lens, Optional, Prism}
import scala.annotation.nowarn
import scala.scalajs.js
import scalacss.ScalaCssReact._

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

    @Lenses
    final case class Initial(items  : Vector[Item[Unit, Unit]],
                             formats: Map[SuiteResultsFormat.Text, Enabled]) extends State

    @Lenses
    final case class Running(items     : Vector[Item[SuiteState, Int]],
                             formats   : Map[SuiteResultsFormat.Text, Enabled],
                             startedAt : js.Date,
                             startedMs : Double,
                             batchPlans: BatchPlans,
                             status    : RunningStatus,
                             abortFn   : Option[AbortFn]) extends State {

      val completedBMs: Int = {
        var x = 0
        def go(items: Vector[Item[State.SuiteState, Int]]): Unit =
          items.foreach {
            case i: Item.Suite[State.SuiteState, Int] =>
              for (s <- i.value)
                x += s.status.runs
            case i: Item.Folder[State.SuiteState, Int] =>
              go(i.children)
          }
        go(items)
        x
      }
    }

    sealed trait RunningStatus
    object RunningStatus {
      case object Running extends RunningStatus
      case object Aborted extends RunningStatus
      case object Complete extends RunningStatus
    }

    type SuiteState = Option[SuiteRunner.State[_]]

    def init(p: Props): State =
      init(p, p.guiOptions.batchModeFormats)

    def init(p: Props, formats: Map[SuiteResultsFormat.Text, Enabled]): State =
      Initial(Item.fromTocItems(p.items), formats)

    val initial: Prism[State, Initial] =
      GenPrism[State, Initial]

    val initialItems: Optional[State, Vector[Item[Unit, Unit]]] =
      initial ^|-> Initial.items

    val running: Prism[State, Running] =
      GenPrism[State, Running]

    val abortFn: Optional[State, Option[AbortFn]] =
      running ^|-> Running.abortFn

    val runningItems: Lens[State, Vector[Item[SuiteState, Int]]] =
      GuiUtil.optionalToLens(running ^|-> State.Running.items)(Vector.empty)

    val runningStatus: Optional[State, RunningStatus] =
      running ^|-> Running.status
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

  final case class BatchPlans(plans        : Vector[BatchPlan],
                              runnableItems: Vector[Item[State.SuiteState, Int]],
                              formats      : Map[SuiteResultsFormat.Text, Enabled]) {

    val enabledFormats: Vector[SuiteResultsFormat.Text] =
      formats.iterator.filter(_._2 is Enabled).map(_._1).toVector

    val newState: CallbackTo[State.Running] =
      CallbackTo {
        State.Running(
          items      = runnableItems,
          formats    = formats,
          startedAt  = new js.Date,
          startedMs  = System.currentTimeMillis().toDouble,
          batchPlans = this,
          status     = State.RunningStatus.Running,
          abortFn    = None,
        )
      }

    def isEmpty: Boolean =
      totalBMs == 0 || enabledFormats.isEmpty

    val totalBMs: Int =
      plans.iterator.map(_.guiPlan.totalBMs).sum

    def start($: StateAccessPure[State], guiOptions: GuiOptions): Callback =
      Callback.byName(startA($, guiOptions).toCallback)

    def startA($: StateAccessPure[State], guiOptions: GuiOptions): AsyncCallback[Unit] = {
      val keepRunning: AsyncCallback[Boolean] =
        $.state.map {
          case s: State.Running => s.status == State.RunningStatus.Running
          case _                => false
        }.asAsyncCallback

      def saveAll[P](done: SuiteRunner.SuiteDone[P], startedAt: js.Date): Callback = {
        val progress = done.progress.copy(startedAt = startedAt)
        val resultFmts = SuiteRunner.deriveResultFmts(progress, done.bm)
        Callback.traverse(enabledFormats) { f =>
          val args = SuiteResultsFormat.Args(
            suite      = done.suite,
            progress   = progress,
            results    = done.bm,
            resultFmts = resultFmts,
            guiOptions = guiOptions,
          )
          f.save(args)
        }
      }

      def runPlan(p: BatchPlan, startedAt: js.Date): AsyncCallback[Unit] =
        for {
          ctls      <- p.start
          _         <- $.modStateAsync(State.abortFn.set(Some(ctls.abortFn)))
          done      <- ctls.onCompletion
          _         <- saveAll(done, startedAt).asAsyncCallback
        } yield ()

      def runPlanUnlessAborted(p: BatchPlan, startedAt: js.Date): AsyncCallback[Unit] =
        keepRunning.flatMap {
          case true  => runPlan(p, startedAt)
          case false => AsyncCallback.unit
        }

      val markAsStarted: AsyncCallback[Unit] =
        newState.asAsyncCallback.flatMap($.setStateAsync)

      val runAll: AsyncCallback[Unit] =
        AsyncCallback.delay(new js.Date).flatMap(startedAt =>
          plans.foldLeft(AsyncCallback.unit)(_ >> runPlanUnlessAborted(_, startedAt)))

      val markAsCompleted: AsyncCallback[Unit] =
        $.modStateAsync(State.runningStatus.modify {
          case State.RunningStatus.Aborted => State.RunningStatus.Aborted
          case _                           => State.RunningStatus.Complete
        })

      markAsStarted >> runAll >> markAsCompleted
    }
  }

  @nowarn("cat=unused") private implicit val reusabilityFormats: Reusability[Map[SuiteResultsFormat.Text, Enabled]] =
    Reusability.byRef

  @nowarn("cat=unused") private implicit val reusabilityStateRunnerState: Reusability[SuiteRunner.State[_]] =
    Reusability.byRef

  @nowarn("cat=unused") private implicit val reusabilityBatchPlan: Reusability[BatchPlan] =
    Reusability.byRef

  @nowarn("cat=unused") private implicit val reusabilityBatchPlans: Reusability[BatchPlans] =
    Reusability.byRef

  @nowarn("cat=unused") private implicit val reusabilityStateRS: Reusability[State.RunningStatus] =
    Reusability.derive

  @nowarn("cat=unused") private implicit val reusabilityStateI: Reusability[State.Initial] =
    Reusability.derive

  @nowarn("cat=unused") private implicit val reusabilityStateR: Reusability[State.Running] = {
    @nowarn("cat=unused") implicit val x: Reusability[Double] = Reusability.by_==
    Reusability.derive
  }

  implicit val reusabilityState: Reusability[State] = Reusability.derive

  // ===================================================================================================================

  final class Backend($: BackendScope[Props, State]) {
    import State.SuiteState

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
            val guiSuite2       = guiSuite.withBMs(enabledBMs)
            val guiPlan         = GuiPlan(guiSuite2)(params)
            val stateAccess     = $$.zoomStateL(runStateLens[P])
            val run             = SuiteRunner.run(guiPlan)(stateAccess, engineOptions)
            val batchPlan       = BatchPlan(guiPlan)(run)
            val initialRunState = SuiteRunner.State.init(guiSuite2, guiOptions, respectDisabledByDefault = false)
            plans += batchPlan
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

      val item2s = loop(initialState.items, $.zoomStateL(State.runningItems))
      BatchPlans(plans.result(), item2s, initialState.formats)
    }

    private val setInitialItems =
      StateSnapshot.withReuse.prepare[Vector[Item[Unit, Unit]]]((oi, cb) =>
        $.modStateOption(s => oi.flatMap(State.initialItems.setOption(_)(s)), cb))

    private val setInitialFormats: Map[SuiteResultsFormat.Text, Enabled] ~=> Callback =
      Reusable.byRef { f =>
        $.modStateOption(_ match {
          case s: State.Initial => Some(s.copy(formats = f))
          case _                => None
        })
      }

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

    private def renderInitial(p: Props, s: State.Initial): VdomNode = {
      val batchPlans = planBatches(s, p.engineOptions, p.guiOptions)
      def startCB    = Reusable.implicitly(s).withLazyValue(batchPlans.start($, p.guiOptions))

      val controls = BatchModeControls.Props(
        completedBMs  = 0,
        bms           = batchPlans.totalBMs,
        elapsedMs     = 0,
        etaMs         = batchPlans.totalBMs * p.engineOptions.estimatedMsPerBM,
        formats       = s.formats,
        updateFormats = Some(setInitialFormats),
        start         = Some(if (batchPlans.isEmpty) None else Some(startCB)),
        abort         = None,
        reset         = None,
        downloadTest  = true,
      )

      val tree = BatchModeTree.Args(
        state          = setInitialItems(s.items),
        renderItem     = initialRenderItem,
        renderBM       = initialRenderBM,
        enabled        = Enabled,
        showCheckboxes = true,
      )

      <.div(*.root,
        controls.render,
        tree.render)
    }

    private def renderRunning(p: Props, s: State.Running): VdomNode = {
      import s.batchPlans

      val elapsedMs    = System.currentTimeMillis().toDouble - s.startedMs
      val remainingBMs = batchPlans.totalBMs - s.completedBMs
      val msPerBM      = if (s.completedBMs > 0) elapsedMs / s.completedBMs else p.engineOptions.estimatedMsPerBM
      val etaMs        = remainingBMs * msPerBM

      val controls: BatchModeControls.Props =
        s.status match {
          case RunningStatus.Running =>

            val abortCB =
              Reusable.implicitly(s.abortFn).withLazyValue(
                $.modState(State.runningStatus.set(State.RunningStatus.Aborted),
                  Callback.traverseOption(s.abortFn)(_.callback)))

            BatchModeControls.Props(
              completedBMs  = s.completedBMs,
              bms           = batchPlans.totalBMs,
              elapsedMs     = elapsedMs,
              etaMs         = etaMs,
              formats       = s.formats,
              updateFormats = None,
              start         = None,
              abort         = Some(abortCB),
              reset         = None,
              downloadTest  = false,
            )

          case RunningStatus.Aborted
             | RunningStatus.Complete =>

            val reset =
              // No other change can happen in this state, no need for Reusability
              Reusable.callbackByRef {
                Callback.byName($.setState(State.init(p, s.formats)))
              }

            BatchModeControls.Props(
              completedBMs  = s.completedBMs,
              bms           = batchPlans.totalBMs,
              elapsedMs     = elapsedMs,
              etaMs         = etaMs,
              formats       = s.formats,
              updateFormats = None,
              start         = None,
              abort         = None,
              reset         = Some(reset),
              downloadTest  = false,
            )
        }

      val tree = BatchModeTree.Args(
        state          = StateSnapshot.withReuse(s.items).readOnly,
        renderItem     = runningRenderItem,
        renderBM       = runningRenderBM,
        enabled        = Disabled,
        showCheckboxes = false,
      )

      <.div(*.root,
        controls.render,
        tree.render)
    }

    def render(p: Props, s: State): VdomNode =
      s match {
        case s: State.Initial => renderInitial(p, s)
        case s: State.Running => renderRunning(p, s)
      }
  }

  val Component = ScalaComponent.builder[Props]
    .initialStateFromProps(State.init)
    .renderBackend[Backend]
    .build
}