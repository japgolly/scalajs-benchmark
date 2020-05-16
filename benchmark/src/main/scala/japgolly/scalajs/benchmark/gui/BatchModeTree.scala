package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.extra.components.TriStateCheckbox
import monocle.{Lens, Traversal}
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import scalaz.Applicative
import scalaz.std.vector._
import scalaz.syntax.traverse._
import Styles.{BatchMode => *}

object BatchModeTree {

  sealed trait Item[A] {
    def name: String
    def bmCount: Int
    def enabledBMs: Int
  }

  object Item {

    @Lenses
    final case class Folder[A](name: String, children: Vector[Item[A]]) extends Item[A] {
      override val bmCount    = children.iterator.map(_.bmCount).sum
      override val enabledBMs = children.iterator.map(_.enabledBMs).sum
    }

    @Lenses
    final case class Suite[A](suite: GuiSuite[_], bms: Vector[BM[A]]) extends Item[A] {
      override def name       = suite.name
      override def bmCount    = bms.length
      override val enabledBMs = bms.count(_.enabled is Enabled)
    }

    @Lenses
    final case class BM[A](enabled: Enabled, value: A)

    implicit def reusabilityB[A: Reusability]: Reusability[BM    [A]] = Reusability.derive
    implicit def reusabilityS[A: Reusability]: Reusability[Suite [A]] = Reusability.derive
    implicit def reusabilityF[A: Reusability]: Reusability[Folder[A]] = Reusability.derive
    implicit def reusability [A: Reusability]: Reusability[Item  [A]] = {
      // TODO https://github.com/japgolly/scalajs-react/issues/717
      Reusability.byRef[Item[A]] || Reusability[Item[A]]((x, y) => x match {
        case i: Folder[A] => y match {
          case j: Folder[A] => i ~=~ j
          case _            => false
        }
        case i: Suite[A] => y match {
          case j: Suite[A] => i ~=~ j
          case _           => false
        }
      })
    }

    def bmsT[A]: Traversal[Item[A], BM[A]] =
      new Traversal[Item[A], BM[A]] {
        override def modifyF[F[_]](f: BM[A] => F[BM[A]])(s: Item[A])(implicit F: Applicative[F]): F[Item[A]] =
          s match {
            case i: Folder[A] => i.children.traverse(modifyF(f)).map(n => i.copy(children = n))
            case i: Suite [A] => i.bms.traverse(f).map(n => i.copy(bms = n))
          }
      }

    private val _enabledT: Traversal[Item[Any], Enabled] =
      bmsT[Any] ^|-> BM.enabled[Any]

    def enabledT[A]: Traversal[Item[A], Enabled] =
      _enabledT.asInstanceOf[Traversal[Item[A], Enabled]]

    private val bmUnitEnabled = BM(Enabled, ())
    private val bmUnitDisabled = BM(Disabled, ())
    private val bmUnit: Enabled => BM[Unit] = {
      case Enabled  => bmUnitEnabled
      case Disabled => bmUnitDisabled
    }

    def fromTocItems(items: Seq[TableOfContents.Item.NonBatchMode]): Vector[Item[Unit]] =
      items.iterator.map {
        case i: TableOfContents.Item.Folder => Folder(i.name, fromTocItems(i.children))
        case i: TableOfContents.Item.Suite  => Suite(i.suite, i.suite.suite.bms.map(bm => bmUnit(Disabled.when(bm.isDisabledByDefault))))
      }.toVector
  }

  // ===================================================================================================================

  final case class Args[A](state  : StateSnapshot[Vector[Item[A]]],
                           enabled: Enabled) {

    def toProps(implicit r: Reusability[Args[A]]): Props =
      Reusable.implicitly(this)

    def render(implicit r: Reusability[Args[A]]): VdomElement =
      Component(toProps)
  }

  object Args {
    implicit def reusability[A: Reusability]: Reusability[Args[A]] =
      Reusability.byRef || Reusability.derive
  }

  type Props = Reusable[Args[_]]

  // ===================================================================================================================

  final class Backend($: BackendScope[Props, Unit]) {
    private val ul = <.ul(*.menuUL)

    private def vectorIndex[A](idx: Int): Lens[Vector[A], A] =
      Lens[Vector[A], A](_(idx))(a => _.patch(idx, a :: Nil, 1))

    def render(p: Props): VdomNode = {

      def triStateCheckbox[A](ss: StateSnapshot[Item[A]]) = {
        val item = ss.value

        val triState =
          if (item.bmCount == 0 || item.enabledBMs == 0)
            TriStateCheckbox.Unchecked
          else if (item.bmCount == item.enabledBMs)
            TriStateCheckbox.Checked
          else
            TriStateCheckbox.Indeterminate

        val setNextState: Callback =
          Callback.byName {
            val nextState: Enabled =
              triState.nextDeterminate match {
                case TriStateCheckbox.Checked   => Enabled
                case TriStateCheckbox.Unchecked => Disabled
              }
            ss.modState(Item.enabledT[A].set(nextState)).when_(p.enabled is Enabled)
          }

        val liStyle =
          *.menuLI(Disabled.when(triState == TriStateCheckbox.Unchecked))

        val label =
          <.label(
            TriStateCheckbox.Props(triState, setNextState).render)

        (liStyle, label)
      }

      def children[A](ss: StateSnapshot[Vector[Item[A]]]): VdomTag =
        ul(
          ss.value.iterator.zipWithIndex.toVdomArray { case (item, idx) =>
            <.li(
              ^.key := idx,
              child(ss.zoomStateL(vectorIndex(idx))))
          }
        )

      def child[A](ss: StateSnapshot[Item[A]]): TagMod = {
        val (liStyle, label) = triStateCheckbox(ss)
        ss.value match {
          case folder: Item.Folder[A] =>
            val ss2 = ss.narrowOption[Item.Folder[A]].get
            TagMod(
              liStyle,
              <.div(label(folder.name)),
              children(ss2.zoomStateL(Item.Folder.children)))

          case i: Item.Suite[A] =>
            val ss2 = ss.narrowOption[Item.Suite[A]].get
            val suite = i.suite.suite
            TagMod(
              liStyle,
              <.div(label(i.suite.name)),
              ul(
                suite.bms.iterator.zipWithIndex.toVdomArray { case (bm, idx) =>
                  <.li(
                    ^.key := idx,
                    benchmark(ss2, idx))
                }
              ))
        }
      }

      def benchmark[A](ss: StateSnapshot[Item.Suite[A]], idx: Int): TagMod = {
        val bm = ss.value.bms(idx)
        def lens = Item.Suite.bms[A] ^|-> vectorIndex(idx) ^|-> Item.BM.enabled
        TagMod(
          *.menuLI(bm.enabled),
          <.label(
            <.input.checkbox(
              ^.checked := bm.enabled.is(Enabled),
              ^.disabled := p.enabled.is(Disabled),
              ^.onChange --> ss.modState(lens.modify(!_)),
            ),
            ss.value.suite.suite.bms(idx).name)
        )
      }

      def all[A](ss1: StateSnapshot[Vector[Item[A]]]): TagMod = {
        val ss = ss1.zoomState(Item.Folder("All", _))(f2 => _ => f2.children)
        child(ss.unsafeWiden[Item[A]])
      }

      <.ul(
        *.menuRootUL,
        <.li(all(p.state)))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}