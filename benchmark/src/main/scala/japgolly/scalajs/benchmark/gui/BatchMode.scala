package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark.Benchmark
import japgolly.scalajs.benchmark.engine.EngineOptions
import japgolly.scalajs.benchmark.gui.TableOfContents.Item
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.components.TriStateCheckbox
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import Styles.{BatchMode => *}

object BatchMode {

  val name = "Batch Mode"

  final case class Props(items        : Seq[Item.NonBatchMode],
                         engineOptions: EngineOptions,
                         guiOptions   : GuiOptions) {

    val deepBmCount: Int =
      items.iterator.map {
        case i: Item.Folder => i.deepBmCount
        case i: Item.Suite  => i.suite.suite.bms.length
      }.sum

    @inline def render: VdomElement = Component(this)
  }

  /** @param suiteId String in the format: { (folderIdx|suiteIdx) ~ ["." ~ (folderIdx|suiteIdx)]* }?
    *                Couldn't be arsed fucking around with Maps, nesting, and emptiness.
    */
  final case class BMKey(suiteId: String, bmIdx: Int)

  sealed trait State
  object State {
    final case class Initial(disabledBMs: Set[BMKey]) extends State
    case object Running extends State
    case object Finished extends State

    def init: State =
      Initial(Set.empty)
  }

  final class Backend($: BackendScope[Props, State]) {

    private def renderItems(p: Props, allDisabledBMs: Set[BMKey]): VdomNode = {
      val ul = <.ul(*.menuUL)

      def deepKeys(items: Seq[Item.NonBatchMode], suiteId: String): Iterator[BMKey] =
        items.iterator.zipWithIndex.flatMap { case (item, idx) =>
          val suiteId2 = if (suiteId.isEmpty) idx.toString else suiteId + "." + idx
          item match {
            case f: Item.Folder => deepKeys(f.children, suiteId2)
            case s: Item.Suite  => s.suite.suite.bms.indices.iterator.map(BMKey(suiteId2, _))
          }
        }

      def allDeepKeys =
        deepKeys(p.items, "")

      def triStateCheckbox(bms            : Int,
                           isChildKey     : BMKey => Boolean,
                           deepKeyIterator: => Iterator[BMKey]) = {

        val disabledBMs =
          allDisabledBMs.iterator.filter(isChildKey).toVector

        val triState =
          if (bms == 0)
            TriStateCheckbox.Unchecked
          else if (disabledBMs.isEmpty)
            TriStateCheckbox.Checked
          else if (disabledBMs.length == bms)
            TriStateCheckbox.Unchecked
          else
            TriStateCheckbox.Indeterminate

        val setNextState: Callback =
          Callback.byName {
            val unrelated = allDisabledBMs.filterNot(isChildKey)
            val newDisabledBMs: Set[BMKey] =
              triState.nextDeterminate match {
                case TriStateCheckbox.Checked   => unrelated
                case TriStateCheckbox.Unchecked => unrelated ++ deepKeyIterator
              }
            $.setState(State.Initial(newDisabledBMs))
          }

//        val soleSelect = Callback.byName {
//          val newDisabledBMs = allDeepKeys.filterNot(isChildKey).toSet
//          $.setState(State.Initial(newDisabledBMs))
//        }

        val liStyle =
          *.menuLI(Disabled.when(triState == TriStateCheckbox.Unchecked))

        val label =
          <.label(
//            ^.onDoubleClick --> soleSelect,
            TriStateCheckbox.Props(triState, setNextState).render)

        (liStyle, label)
      }

      def children(items: Seq[Item.NonBatchMode], suiteIdPrefix: String): VdomTag =
        ul(
          items.iterator.zipWithIndex.toVdomArray { case (item, idx) =>
            <.li(
              ^.key := idx,
              child(item, suiteIdPrefix + idx))
          }
        )

      def child(item: Item.NonBatchMode, suiteId: String): TagMod =
        item match {

          case folder: Item.Folder =>
            val suiteIdPrefix = suiteId + "."
            val (liStyle, label) = triStateCheckbox(
              bms             = folder.deepBmCount,
              isChildKey      = _.suiteId.startsWith(suiteIdPrefix),
              deepKeyIterator = deepKeys(folder.children, suiteId),
            )

            TagMod(
              liStyle,
              <.div(label(folder.name)),
              children(folder.children, suiteIdPrefix))

          case i: Item.Suite =>
            val suite = i.suite.suite
            val (liStyle, label) = triStateCheckbox(
              bms             = suite.bms.length,
              isChildKey      = _.suiteId == suiteId,
              deepKeyIterator = suite.bms.indices.iterator.map(BMKey(suiteId, _)),
            )

            TagMod(
              liStyle,
              <.div(label(i.suite.name)),
              ul(
                suite.bms.iterator.zipWithIndex.toVdomArray { case (bm, idx) =>
                  <.li(
                    ^.key := idx,
                    benchmark(bm, BMKey(suiteId, idx)))
                }
              ))
        }

      def benchmark(bm: Benchmark[_], key: BMKey): TagMod = {
        val enabled = Disabled.when(allDisabledBMs.contains(key))
        val toggle = Callback.byName {
          val newDisabledBMs: Set[BMKey] =
            enabled match {
              case Enabled  => allDisabledBMs + key
              case Disabled => allDisabledBMs - key
            }
          $.setState(State.Initial(newDisabledBMs))
        }
//        val soleSelect = Callback.byName {
//          val newDisabledBMs = allDeepKeys.toSet - key
//          $.setState(State.Initial(newDisabledBMs))
//        }
        TagMod(
          *.menuLI(enabled),
          <.label(
//            ^.onDoubleClick --> soleSelect,
            <.input.checkbox(
              ^.checked := enabled.is(Enabled),
              ^.onChange --> toggle,
            ),
            bm.name
          )
        )
      }

      def all: TagMod = {
        // Not very efficient but it's negligible given the size of BM suites
        val (liStyle, label) = triStateCheckbox(
          bms             = p.deepBmCount,
          isChildKey      = _ => true,
          deepKeyIterator = allDeepKeys,
        )
        TagMod(
          liStyle,
          <.div(label("All")),
          children(p.items, ""))
      }

      <.ul(
        *.menuRootUL,
        <.li(all))
    }

    private def renderInitial(p: Props, s: State.Initial): VdomNode = {
      renderItems(p, s.disabledBMs)
    }

    def render(p: Props, s: State): VdomNode =
      s match {
        case s: State.Initial => renderInitial(p, s)
        case State.Running
           | State.Finished => "?"
      }
  }

  val Component = ScalaComponent.builder[Props]
    .initialState(State.init)
    .renderBackend[Backend]
    .build
}