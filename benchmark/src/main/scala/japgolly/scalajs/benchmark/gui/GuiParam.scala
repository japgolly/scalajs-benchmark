package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react._, vdom.html_<^._
import scala.collection.immutable.BitSet
import scalacss.ScalaCssReact._
import Styles.{Suite => *}

final class GuiParam[A, B](val header    : Header,
                           val initValues: Vector[A],
                           val render    : Render[A],
                           val renderTxt : RenderTxt[A],
                           val editor    : Editor[B],
                           val parser    : Parser[A, B])

object GuiParam {

  def apply[A, B](render   : Render[A],
                  renderTxt: RenderTxt[A],
                  editor   : Editor[B],
                  parser   : Parser[A, B])
                 (header   : Header, initValues: A*): GuiParam[A, B] =
    new GuiParam(header, initValues.toVector, render, renderTxt, editor, parser)

  implicit def autoSingleParam[A, B](p: GuiParam[A, B]): GuiParams[A] =
    GuiParams.one(p)

  def enum[A](header: Header, values: A*)
             (resultTxt    : RenderTxt[A],
              resultLabel  : Render[A] = null,
              editorLabel  : A => VdomElement = null,
              initialValues: Seq[A] = null): GuiParam[A, BitSet] = {

    val resultLabel2: Render[A] =
      Option(resultLabel).getOrElse(resultTxt.andThen(s => s))

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

    val renderInEditor: A => VdomElement =
      Option(editorLabel) getOrElse (a => <.div(^.display.inline, resultLabel2(a)))

    val editor: Editor[BitSet] =
      e => {
        def toggleBM(i: Int): Callback =
          e.modState(s => if (s contains i) s - i else s + i)

        def makeSoleBM(i: Int): Callback =
          e.setState(BitSet.empty + i)

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
          }.toVdomArray)
      }

    val init: Seq[A] =
      Option(initialValues) getOrElse vs

    GuiParam(resultLabel2, resultTxt, editor, parser)(header, init: _*)
  }

  def boolean(header: Header): GuiParam[Boolean, BitSet] =
    enum(header, true, false)(RenderTxt.Bool, Render.Bool)

  def int(header: Header, initialValues: Int*): GuiParam[Int, String] =
    GuiParam(Render.Int, RenderTxt.Int, Editor.Text, Parser.IntsAsText)(header, initialValues: _*)
}
