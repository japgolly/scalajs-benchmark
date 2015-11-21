package japgolly.scalajs.benchmark.gui

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
}
