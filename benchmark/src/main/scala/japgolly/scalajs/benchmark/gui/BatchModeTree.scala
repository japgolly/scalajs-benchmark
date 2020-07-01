package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.gui.Styles.{BatchMode => *}
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.extra.components.TriStateCheckbox
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Traversal
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import scalaz.Applicative
import scalaz.std.vector._
import scalaz.syntax.traverse._

object BatchModeTree {

  sealed trait Item[A, B] {
    def name: String
    def bmCount: Int
    def enabledBMs: Int
  }

  object Item {

    @Lenses
    final case class Folder[A, B](name: String, children: Vector[Item[A, B]]) extends Item[A, B] {
      override val bmCount    = children.iterator.map(_.bmCount).sum
      override val enabledBMs = children.iterator.map(_.enabledBMs).sum
    }

    @Lenses
    final case class Suite[A, B](suite: GuiSuite[_], bms: Vector[BM[B]], value: A) extends Item[A, B] {
      override def name       = suite.name
      override def bmCount    = bms.length
      override val enabledBMs = bms.count(_.enabled is Enabled)
    }

    @Lenses
    final case class BM[B](enabled: Enabled, value: B)

    implicit def reusabilityB[                B: Reusability]: Reusability[BM    [   B]] = Reusability.derive
    implicit def reusabilityS[A: Reusability, B: Reusability]: Reusability[Suite [A, B]] = Reusability.derive
    implicit def reusabilityF[A: Reusability, B: Reusability]: Reusability[Folder[A, B]] = Reusability.derive
    implicit def reusability [A: Reusability, B: Reusability]: Reusability[Item  [A, B]] = Reusability.derive

    def bmsT[A, B]: Traversal[Item[A, B], BM[B]] =
      new Traversal[Item[A, B], BM[B]] {
        override def modifyF[F[_]](f: BM[B] => F[BM[B]])(s: Item[A, B])(implicit F: Applicative[F]): F[Item[A, B]] =
          s match {
            case i: Folder[A, B] => i.children.traverse(modifyF(f)).map(n => i.copy(children = n))
            case i: Suite [A, B] => i.bms.traverse(f).map(n => i.copy(bms = n))
          }
      }

    private val _enabledT: Traversal[Item[Any, Any], Enabled] =
      bmsT[Any, Any] ^|-> BM.enabled[Any]

    def enabledT[A, B]: Traversal[Item[A, B], Enabled] =
      _enabledT.asInstanceOf[Traversal[Item[A, B], Enabled]]

    private val bmUnitEnabled = BM(Enabled, ())
    private val bmUnitDisabled = BM(Disabled, ())
    private val bmUnit: Enabled => BM[Unit] = {
      case Enabled  => bmUnitEnabled
      case Disabled => bmUnitDisabled
    }

    def fromTocItems(items: Seq[TableOfContents.Item.NonBatchMode]): Vector[Item[Unit, Unit]] =
      items.iterator.map {
        case i: TableOfContents.Item.Folder => Folder(i.name, fromTocItems(i.children))
        case i: TableOfContents.Item.Suite  => Suite(i.suite, i.suite.suite.bms.map(bm => bmUnit(Disabled.when(bm.isDisabledByDefault))), ())
      }.toVector
  }

  // ===================================================================================================================

  final case class RenderBM[A, B](suiteItem: Item.Suite[A, B], idx: Int) {
    def bmItem   = suiteItem.bms(idx)
    def guiSuite = suiteItem.suite
    def bm       = guiSuite.suite.bms(idx)
    def name     = bm.name
  }

  final case class Args[A, B](state         : StateSnapshot[Vector[Item[A, B]]],
                              renderItem    : Item[A, B] ~=> VdomNode,
                              renderBM      : RenderBM[A, B] ~=> VdomNode,
                              enabled       : Enabled,
                              showCheckboxes: Boolean) {

    def toProps(implicit r: Reusability[Args[A, B]]): Props =
      Reusable.implicitly(this)

    def render(implicit r: Reusability[Args[A, B]]): VdomElement =
      Component(toProps)
  }

  object Args {
    implicit def reusability[A: Reusability, B: Reusability]: Reusability[Args[A, B]] =
      Reusability.byRef || Reusability.derive
  }

  type Props = Reusable[Args[_, _]]

  // ===================================================================================================================

  final class Backend {
    private val ul = <.ul(*.menuUL)

    def render(p: Props): VdomNode =
      renderA(p.value)

    private def renderA[A, B](p: Args[A, B]): VdomNode = {

      def triStateCheckbox(ss: StateSnapshot[Item[A, B]]) = {
        val item = ss.value

        val triState =
          if (item.bmCount == 0 || item.enabledBMs == 0)
            TriStateCheckbox.Unchecked
          else if (item.bmCount == item.enabledBMs)
            TriStateCheckbox.Checked
          else
            TriStateCheckbox.Indeterminate

        def setNextState: Callback =
          Callback.byName {
            val nextState: Enabled =
              triState.nextDeterminate match {
                case TriStateCheckbox.Checked   => Enabled
                case TriStateCheckbox.Unchecked => Disabled
              }
            ss.modState(Item.enabledT[A, B].set(nextState)).when_(p.enabled is Enabled)
          }

        val liStyle =
          *.menuLI(Disabled.when(triState == TriStateCheckbox.Unchecked))

        val label =
          <.label(
            TagMod.when(p.showCheckboxes)(
              TriStateCheckbox.Props(triState, setNextState).render))

        (liStyle, label)
      }

      def children(ss: StateSnapshot[Vector[Item[A, B]]]): VdomTag =
        ul(
          ss.value.indices.toVdomArray { idx =>
            <.li(
              ^.key := idx,
              child(ss.zoomStateL(GuiUtil.vectorIndex(idx))))
          }
        )

      def child(ss: StateSnapshot[Item[A, B]]): TagMod = {
        val (liStyle, label) = triStateCheckbox(ss)
        ss.value match {
          case folder: Item.Folder[A, B] =>
            val ss2 = ss.narrowOption[Item.Folder[A, B]].get // safe because we just patmat'd on it
            TagMod(
              liStyle,
              <.div(label(p.renderItem(folder))),
              children(ss2.zoomStateL(Item.Folder.children)))

          case i: Item.Suite[A, B] =>
            val ss2 = ss.narrowOption[Item.Suite[A, B]].get // safe because we just patmat'd on it
            val suite = i.suite.suite
            val isValid = i.suite.defaultParams.isRight
            TagMod(
              liStyle,
              <.div(label(p.renderItem(i))),
              ul(
                suite.bms.indices.toVdomArray { idx =>
                  <.li(
                    ^.key := idx,
                    benchmark(ss2, idx, isValid))
                }
              ))
        }
      }

      def benchmark(ss: StateSnapshot[Item.Suite[A, B]], idx: Int, isValid: Boolean): TagMod = {
        val bm = ss.value.bms(idx)
        val enabled = bm.enabled & Enabled.when(isValid)
        val editing = p.enabled & Enabled.when(isValid)
        def lens = Item.Suite.bms[A, B] ^|-> GuiUtil.vectorIndex(idx) ^|-> Item.BM.enabled
        TagMod(
          *.menuLI(enabled),
          <.label(
            TagMod.when(p.showCheckboxes)(
              <.input.checkbox(
                ^.checked := enabled.is(Enabled),
                ^.disabled := editing.is(Disabled),
                ^.onChange --> Callback.byName(ss.modState(lens.modify(!_))),
              )
            ),
            p.renderBM(RenderBM(ss.value, idx))))
      }

      def all(ss1: StateSnapshot[Vector[Item[A, B]]]): TagMod = {
        val ss = ss1.zoomState(Item.Folder("All", _))(f2 => _ => f2.children)
        child(ss.unsafeWiden[Item[A, B]])
      }

      <.section(
        <.h3("Benchmarks"),
        <.ul(*.menuRootUL,
          <.li(all(p.state))))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}