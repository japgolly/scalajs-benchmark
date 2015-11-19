package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.ExternalVar
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.Prism
import scalacss.ScalaCssReact._
import scalaz.\/
import scalaz.std.option._
import scalaz.std.vector._
import scalaz.syntax.traverse._

import Param._

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

  type Header = String

  // ===================================================================================================================

  type Render[-A] = A => TagMod

  object Render {
    def int: Render[Int] =
      i => TagMod(Styles.Suite.paramInt, i)

    def bool: Render[Boolean] =
      b => TagMod(Styles.Suite.paramBool, if (b) "T" else "F")

  }

  // ===================================================================================================================

  type Editor[A] = ExternalVar[A] => ReactElement

  object Editor {
    val text: Editor[String] =
      e =>
        <.input(
          ^.`type` := "text",
          ^.value := e.value,
          ^.onChange ==> ((i: ReactEventI) => e.set(i.target.value)))
  }

  type Parser[A, B] = Prism[B, Vector[A]]

  // ===================================================================================================================

  object Parser {
    val intsAsText: Parser[Int, String] =
      Prism[String, Vector[Int]](
        _.split("[ ,]")
          .iterator
          .map(_.trim)
          .filter(_.nonEmpty)
          .map(is => \/.fromTryCatchNonFatal(is.toInt).toOption)
          .toVector
          // .distinct
          .sequence
      )(_ mkString ", ")

    val boolsAsText: Parser[Boolean, String] =
      Prism[String, Vector[Boolean]](
        _.split("[ ,]")
          .iterator
          .map(_.trim.toLowerCase)
          .filter(_.nonEmpty)
          .map {
            case "t" | "true" | "yes" | "y" | "1" => Some(true)
            case "f" | "false" | "no" | "n" | "0" => Some(false)
            case _ => None
          }
          .toVector
          // .distinct
          .sequence
      )(_ mkString ", ")

  }

}
