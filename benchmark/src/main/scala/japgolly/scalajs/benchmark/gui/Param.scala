package japgolly.scalajs.benchmark.gui

final class Param[A, B](val header    : Header,
                        val initValues: Vector[A],
                        val render    : Render[A],
                        val editor    : Editor[B],
                        val parser    : Parser[A, B])

object Param {

  def apply[A, B](render: Render[A],
                  editor: Editor[B],
                  parser: Parser[A, B])
                 (header: Header, initValues: A*): Param[A, B] =
    new Param(header, initValues.toVector, render, editor, parser)

  implicit def autoSingleParam[A, B](p: Param[A, B]): Params[A] =
    Params.one(p)
}
