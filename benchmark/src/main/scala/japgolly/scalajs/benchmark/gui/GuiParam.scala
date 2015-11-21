package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react._, vdom.prefix_<^._
import scala.collection.immutable.BitSet
import scalacss.ScalaCssReact._
import Styles.{Suite => *}

final class GuiParam[A, B](val header    : Header,
                           val initValues: Vector[A],
                           val render    : Render[A],
                           val editor    : Editor[B],
                           val parser    : Parser[A, B])

object GuiParam {

  def apply[A, B](render: Render[A],
                  editor: Editor[B],
                  parser: Parser[A, B])
                 (header: Header, initValues: A*): GuiParam[A, B] =
    new GuiParam(header, initValues.toVector, render, editor, parser)

  implicit def autoSingleParam[A, B](p: GuiParam[A, B]): GuiParams[A] =
    GuiParams.one(p)

  def enum[A](header: Header, values: TraversableOnce[A])
             (resultLabel: Render[A],
              editorLabel: A => ReactElement = null,
              initialValues: Seq[A] = null): GuiParam[A, BitSet] = {

    val vs = values.toVector
    val vi = vs.zipWithIndex

    val parser =
      Parser[A, BitSet](
        av => {
          val as = av.toSet
          BitSet.empty ++ vi.iterator.filter(as contains _._1).map(_._2)
        }
      )(bs => Some(
        vi.iterator.filter(bs contains _._2).map(_._1).toVector
      ))

    val renderInEditor: A => ReactElement =
      Option(editorLabel) getOrElse (a => <.div(^.display.inline, resultLabel(a)))

    val editor: Editor[BitSet] =
      e => {
        def toggleBM(i: Int): Callback =
          e.mod(s => if (s contains i) s - i else s + i)

        def makeSoleBM(i: Int): Callback =
          e.set(BitSet.empty + i)

        <.div(
          vi.iterator.map { case (a,i) =>
            <.label(
              *.paramEnumLabel,
              ^.key := i,
              ^.onDoubleClick --> makeSoleBM(i),
              <.input(
                ^.`type`         := "checkbox",
                ^.checked        := e.value.contains(i),
                ^.onChange      --> toggleBM(i)),
              renderInEditor(a))
          }.toReactNodeArray)
      }

    val init: Seq[A] =
      Option(initialValues) getOrElse vs

    GuiParam(resultLabel, editor, parser)(header, init: _*)
  }

  def boolean(header: Header): GuiParam[Boolean, BitSet] =
    enum(header, true :: false :: Nil)(Render.Bool)
}
