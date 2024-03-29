package japgolly.scalajs.benchmark

import cats.instances.option._
import cats.instances.vector._
import cats.syntax.traverse._
import japgolly.microlibs.stdlib_ext.ParseInt
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.{Iso, Prism}
import scalacss.ScalaCssReact._

package object gui {

  val CssSettings = scalacss.devOrProdDefaults

  type Header = String

  // ===================================================================================================================

  type Render[-A] = A => TagMod

  object Render {

    val Int: Render[Int] =
      i => TagMod(Styles.Suite.paramInt, i)

    val Bool: Render[Boolean] =
      b => TagMod(Styles.Suite.paramBool, if (b) "T" else "F")
  }

  // ===================================================================================================================

  type RenderTxt[-A] = A => String

  object RenderTxt {

    val Int: RenderTxt[Int] =
      GuiUtil.prettyPrintNumber(_)

    val Bool: RenderTxt[Boolean] =
      b => if (b) "T" else "F"
  }

  // ===================================================================================================================

  type Editor[A] = StateSnapshot[A] => VdomElement

  object Editor {

    val Text: Editor[String] =
      e =>
        <.input.text(
          ^.value := e.value,
          ^.onChange ==> ((i: ReactEventFromInput) => e.setState(i.target.value)))
  }

  // ===================================================================================================================

  type Parser[A, B] = Prism[B, Vector[A]]

  object Parser {

    def apply[A, B](f: Vector[A] => B)(g: B => Option[Vector[A]]): Parser[A, B] =
      Prism[B, Vector[A]](g)(f)

    type TextSeparator = Iso[Vector[String], String]

    def TextSeparator(f: Vector[String] => String, g: String => Vector[String]): TextSeparator =
      Iso(f)(g)

    val SepTextByCommaOrSpace: TextSeparator = {
      val r = "[ ,]".r
      TextSeparator(_ mkString ", ", r.split(_).toVector)
    }

    def listAsText[A](prism: Prism[String, A],
                    sep: TextSeparator = SepTextByCommaOrSpace): Parser[A, String] =
      Parser[A, String](
        va => sep get va.map(prism.reverseGet))(
        sep.reverseGet(_)
          .iterator
          .map(_.trim)
          .filter(_.nonEmpty)
          .map(prism.getOption)
          .toVector
          // .distinct
          .sequence
      )

    val IntStringPrism: Prism[String, Int] =
      Prism[String, Int](ParseInt.unapply)(_.toString)

    val IntsAsText: Parser[Int, String] =
      listAsText(IntStringPrism)

    val BoolStringPrism: Prism[String, Boolean] =
      Prism[String, Boolean](_.toLowerCase match {
        case "t" | "true" | "yes" | "y" | "1" => Some(true)
        case "f" | "false" | "no" | "n" | "0" => Some(false)
        case _ => None
      })(_.toString)
  }
}
