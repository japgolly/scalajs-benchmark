package japgolly.scalajs.benchmark

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.ExternalVar
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.{Iso, Prism}
import scalacss.ScalaCssReact._
import scalaz.\/
import scalaz.std.option._
import scalaz.std.vector._
import scalaz.syntax.traverse._

package object gui {

  type Header = String

  // ===================================================================================================================

  type Render[-A] = A => TagMod

  object Render {
    @deprecated("Use Int instead.", "0.2.0") def int = Int
    @deprecated("Use Bool instead.", "0.2.0") def bool = Bool

    val Int: Render[Int] =
      i => TagMod(Styles.Suite.paramInt, i)

    val Bool: Render[Boolean] =
      b => TagMod(Styles.Suite.paramBool, if (b) "T" else "F")
  }

  // ===================================================================================================================

  type Editor[A] = ExternalVar[A] => ReactElement

  object Editor {
    @deprecated("Use Text instead.", "0.2.0") def text = Text

    val Text: Editor[String] =
      e =>
        <.input(
          ^.`type` := "text",
          ^.value := e.value,
          ^.onChange ==> ((i: ReactEventI) => e.set(i.target.value)))
  }

  // ===================================================================================================================

  type Parser[A, B] = Prism[B, Vector[A]]

  object Parser {
    @deprecated("Use IntsAsText instead.", "0.2.0") def intsAsText = IntsAsText
    @deprecated("Use BoolsAsText instead.", "0.2.0") def boolsAsText = BoolsAsText

    type TextSeparator = Iso[Vector[String], String]

    def TextSeparator(f: Vector[String] => String, g: String => Vector[String]): TextSeparator =
      Iso(f)(g)

    val sepTextByCommaOrSpace: TextSeparator = {
      val r = "[ ,]".r
      TextSeparator(_ mkString ", ", r.split(_).toVector)
    }

    val intStringPrism: Prism[String, Int] =
      Prism[String, Int](s => \/.fromTryCatchNonFatal(s.toInt).toOption)(_.toString)

    def listAsText[A](prism: Prism[String, A],
                    sep: TextSeparator = sepTextByCommaOrSpace): Parser[A, String] =
      Prism[String, Vector[A]](
        sep.reverseGet(_)
          .iterator
          .map(_.trim)
          .filter(_.nonEmpty)
          .map(prism.getOption)
          .toVector
          // .distinct
          .sequence
      )(va => sep get va.map(prism.reverseGet))


    val IntsAsText: Parser[Int, String] =
      listAsText(intStringPrism)

    val BoolsAsText: Parser[Boolean, String] =
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
