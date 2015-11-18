package whatever

import monocle._
import java.util.concurrent.TimeUnit
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.ExternalVar
import japgolly.scalajs.react.vdom.prefix_<^._
import scala.concurrent.duration._
import scalacss.ScalaCssReact._
import scalaz.{Lens => _, _}, Scalaz.{^ => _, _}
import japgolly.scalajs.benchmark.gui.Styles

object NotMyProb {
  type Header       = String
  type RenderValue[-A]   = A => TagMod
  //  type Defaults[+A] = Vector[A]
  type Editor[S] = ExternalVar[S] => ReactElement

  type Key[T] = Lens[GenState, T]
  object Key {
    private var global = ' '

    def apply[T](): Key[T] = {
      global = (global + 1).toChar
      val key = global
      Lens[GenState, T](_.apply(key).asInstanceOf[T])(t => _.updated(key, t))
    }
  }

  type GenState = Map[Char, Any]
  type GenEditor = Editor[GenState]

  case class Param[A, B](header: Header, renderValue: RenderValue[A], editor: Editor[B], initValues: Vector[A],
                         prism: Prism[B, Vector[A]])

  trait ParamWithKey {
    type A
    type B
    val param: Param[A, B]
    val key: Key[B]

    val editor: GenEditor =
      e => param editor ExternalVar(key get e.value)(b => e.set(key.set(b)(e.value)))

    def parseEditorState(b: B): Option[Vector[A]] =
      param.prism.getOption(b)
  }
  object ParamWithKey {
    type Aux[X, Y] = ParamWithKey {type A =X; type B = Y}

    def apply[X, Y](p: Param[X, Y]): Aux[X, Y] =
      new ParamWithKey {
        override type A = X
        override type B = Y
        override val param = p
        override val key = Key[B]()
      }
  }

  trait Params[P] {
    def forState(s: GenState): Header \/ Vector[P]
    def initState: GenState
    def renderHE: Vector[(Header, GenEditor)]
    def renderValues(p: P): Vector[TagMod]
  }

  object Params {

    def single[P, E](param: Param[P, E]): Params[P] =
      new Params[P] {
        val p = ParamWithKey(param)

        override def forState(s: GenState) = {
          p.parseEditorState(p.key.get(s)) match {
            case None => -\/(p.param.header)
            case Some(x) => \/-(x)
          }
        }

        override def initState: GenState =
          p.key.set(p.param.prism reverseGet p.param.initValues)(Map.empty)

        override def renderHE: Vector[(Header, GenEditor)] =
          Vector((param.header, p.editor))

        override def renderValues(p: P): Vector[TagMod] =
          Vector(param renderValue p)
      }

    def two[P, P1, E1, P2, E2](iso: Iso[P, (P1, P2)], param1: Param[P1, E1], param2: Param[P2, E2]): Params[P] =
      new Params[P] {
        import monocle.function.{first, second}
        import monocle.std.tuple2._

//        val p1 = ParamWithKey(param1)
//        val p2 = ParamWithKey(param2)
//        val ps = Vector[ParamWithKey](p1, p2)
        val p1 = ParamWithKey2(param1, iso ^|-> first)
        val p2 = ParamWithKey2(param2, iso ^|-> second)
        val ps = Vector[ParamWithKey2[P]](p1, p2)

        override def forState(s: GenState): Header \/ Vector[P] = {

//          val x =
            for {
              v1 <- p1.parseEditorState(p1.key.get(s)) \/> p1.param.header
              v2 <- p2.parseEditorState(p2.key.get(s)) \/> p2.param.header
            } yield (
              for {a1 <- v1; a2 <- v2} yield iso.reverseGet((a1, a2))
            )

//          for {
//            v1 <- p1.parseEditorState(p1.key.get(s)).toRight(p1.param.header)
//            v2 <- p2.parseEditorState(p2.key.get(s)).toRight(p2.param.header)
//            a1 <- v1; a2 <- v2
//          } yield iso.reverseGet((a1, a2))

        }

        override def initState: GenState =
          ps.foldLeft(Map.empty: GenState)((m, p) =>
            p.key.set(p.param.prism reverseGet p.param.initValues)(m))

        override def renderHE: Vector[(Header, GenEditor)] =
          ps.map(p => (p.param.header, p.editor))

        override def renderValues(v: P): Vector[TagMod] =
          ps.map(p => p.param renderValue p.lens.get(v))
      }

    trait ParamWithKey2[P] {
      type A
      type B
      val param: Param[A, B]
      val key: Key[B]
      val lens: Lens[P, A]
      val editor: GenEditor =
        e => param editor ExternalVar(key get e.value)(b => e.set(key.set(b)(e.value)))
      def parseEditorState(b: B): Option[Vector[A]] =
        param.prism.getOption(b)
    }
    object ParamWithKey2 {
      type Aux[P, X, Y] = ParamWithKey2[P] {type A =X; type B = Y}

      def apply[P, X, Y](p: Param[X, Y], _lens: Lens[P, X]): Aux[P, X, Y] =
        new ParamWithKey2[P] {
          override type A = X
          override type B = Y
          override val param = p
          override val key = Key[B]()
          override val lens = _lens
        }
    }
  }

  // ===================================================================================================================

  def renderInt: RenderValue[Int] =
    i => TagMod(Styles.ResultTable.paramInt, i)

  def renderBool: RenderValue[Boolean] =
    b => TagMod(Styles.ResultTable.paramBool, if (b) "T" else "F")

  def textEditor: Editor[String] =
    e =>
      <.input(
        ^.`type` := "text",
        ^.value := e.value,
        ^.onChange ==> ((i: ReactEventI) => e.set(i.target.value)))

  val intsToText = Prism[String, Vector[Int]](
              _.split("[ ,]")
                .iterator
                .map(_.trim)
                .filter(_.nonEmpty)
                .map(is => \/.fromTryCatchNonFatal(is.toInt).toOption)
                .toVector
//                .distinct
                .sequence
  )(_ mkString ", ")

  val boolsToText = Prism[String, Vector[Boolean]](
              _.split("[ ,]")
                .iterator
                .map(_.trim.toLowerCase)
                .filter(_.nonEmpty)
                .map{
                  case "t" | "true" | "y" | "yes" | "1" => Some(true)
                  case "f" | "false" | "n" | "no" | "0" => Some(false)
                  case _ => None
                }
                .toVector
//                .distinct
                .sequence
  )(_ mkString ", ")
}