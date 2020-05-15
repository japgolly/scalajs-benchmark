package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.{Iso, Lens}
import scalaz.{\/-, \/}
import scalaz.std.option.optionSyntax._

trait GuiParams[P] {
  import GuiParams._

  def initialState: GenState

  def parseState(s: GenState): ParseResult[P]

  def headers: Vector[Header]

  def editors: Vector[GenEditor]

  def renderParams(p: P): Vector[TagMod]

  def renderParamsToText(p: P): Vector[String]

  def bmNameSuffix(p: P): String =
    " @ " + p.toString
}

object GuiParams extends GuiParamsBoilerplate {
  type GenState = Vector[Any]
  type ParseResult[P] = Header \/ Vector[P]
  type GenEditor = Editor[GenState]

  import Internals._

  val none: GuiParams[Unit] =
    new GuiParams[Unit] {
      override def initialState                = Vector.empty
      override def headers                     = Vector.empty
      override def editors                     = Vector.empty
      override def renderParams(p: Unit)       = Vector.empty
      override def renderParamsToText(p: Unit) = Vector.empty
      override def bmNameSuffix(p: Unit)       = ""
      override def parseState(s: GenState)     = parseResult
      val parseResult = \/-(Vector(()))
    }

  def one[P, E](param: GuiParam[P, E]): GuiParams[P] = {
    val p = SubParam(0, param, Lens.id[P])
    val ps = Vector(p)

    new MostlyGenericParams(ps) {
      override def parseState(s: GenState): ParseResult[P] =
        p.parse(p.key.get(s)) \/> p.param.header
    }
  }

  @deprecated("Use .combine2", "0.7.0")
  def two[P, P1, E1, P2, E2](iso: Iso[P, (P1, P2)], param1: GuiParam[P1, E1], param2: GuiParam[P2, E2]): GuiParams[P] =
    combine2(iso)(param1, param2)

  // ===================================================================================================================

  protected object Internals {

    def emptyState(size: Int): GenState =
      Vector.fill(size)(())

    type Key[T] = Lens[GenState, T]

    def Key[T](stateIndex: Int): Key[T] =
      Lens[GenState, T](_.apply(stateIndex).asInstanceOf[T])(v => _.updated(stateIndex, v))

    trait SubParam[P] {
      type A
      type B
      val lens: Lens[P, A]
      val param: GuiParam[A, B]
      val key: Key[B]

      val editor: GenEditor =
        e => param editor StateSnapshot(key get e.value)((ob, cb) => e.setStateOption(ob.map(key.set(_)(e.value)), cb))

      def parse(b: B): Option[Vector[A]] =
        param.parser.getOption(b)
    }

    object SubParam {
      type Aux[P, X, Y] = SubParam[P] {type A = X; type B = Y}

      def apply[P, X, Y](stateIndex: Int, p: GuiParam[X, Y], _lens: Lens[P, X]): Aux[P, X, Y] =
        new SubParam[P] {
          override type A = X
          override type B = Y
          override val param = p
          override val key = Key[B](stateIndex)
          override val lens = _lens
        }
    }

    /**
      * MostlyGeneric cos I'm tired and I suck.
      *
      * TODO Use a HList
      */
    abstract class MostlyGenericParams[P](ps: Vector[SubParam[P]]) extends GuiParams[P] {
      override final def initialState: GenState =
        ps.foldLeft(emptyState(ps.length))((m, p) =>
          p.key.set(p.param.parser reverseGet p.param.initValues)(m))

      override final val headers: Vector[Header] =
        ps.map(_.param.header)

      override final val editors: Vector[GenEditor] =
        ps.map(_.editor)

      override final def renderParams(i: P): Vector[TagMod] =
        ps.map(p => p.param render p.lens.get(i))

      override final def renderParamsToText(i: P): Vector[String] =
        ps.map(p => p.param renderTxt p.lens.get(i))
    }
  }
}
