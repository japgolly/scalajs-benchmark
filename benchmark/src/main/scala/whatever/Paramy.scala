package whatever

import monocle._
import java.util.concurrent.TimeUnit
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.ExternalVar
import japgolly.scalajs.react.vdom.prefix_<^._
import scala.concurrent.duration._
import scalacss.ScalaCssReact._
import scalaz.\/

//
//object Paramy {
//
//  type Header       = String
//  type Render[-A]   = A => TagMod
//  type Defaults[+A] = Vector[A]
//
//
//  /*
////  class ParamFmt[-A](val headers: Vector[String], val render: A => Vector[TagMod])
//
//  case class FmtParam[-A](header: String, render: A => TagMod) {
////    def :+[B <: A](b: FmtParam[B]): FmtParams[B] =
////      Vector.empty :+ this :+ b
//  }
//*/
//
//  case class Param[P, A](header: Header,
//                         render: Render[P],
//                         defaults: Defaults[A]) {
//
//    def cmap[B](f: B => P): Param[B, A] =
//      Param(header, render compose f, defaults)
//
//  }
//
//
////  type Params[A] = Vector[Param[A]]
////
////  implicit def liftOneParam[A](p: Param[A]): Params[A] =
////    Vector.empty :+ p
//
//  /*
//
//
//  val ps = suite.params
//  for p <- ps
//    render p.header
//    p.render(op state, op state -> Callback) <---- state is of type p.T
//
//
//  // run benchmarks
//  Vector P params = ps.gimme(state)
//
//
//  ONE
//  state = Vector[Bool]
//
//  STATE.getState(key): Op value: key.T
//   */
//
//
////  type PDState
//
//
//
//  // ===================================================================================================================
//
//  def renderInt: Render[Int] =
//    i => TagMod(Styles.ResultTable.paramInt, i)
//
//  def renderBool: Render[Boolean] =
//    b => TagMod(Styles.ResultTable.paramBool, if (b) "T" else "F")
//
//}


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

  case class Param[A, B](header: Header, renderValue: RenderValue[A], editor: Editor[B], initValues: Vector[A],
                         bas: Prism[B, Vector[A]])

  trait ParamWithKey[A] {
    type B
    val param: Param[A, B]
    val key: Key[B]

    def editor(e: ExternalVar[GenState]): TagMod =
      param editor ExternalVar(key get e.value)(b => e.set(key.set(b)(e.value)))

    def parseEditorState(b: B): Option[Vector[A]] =
      param.bas.getOption(b)
  }

  trait Params[P] {
    val paramDefs: Vector[ParamWithKey[P]]
    val forState: GenState => ParamWithKey[P] \/ Vector[P]

    def initState: GenState =
      paramDefs.foldLeft(Map.empty: GenState)((m, p) =>
        p.key.set(p.param.bas reverseGet p.param.initValues)(m))

  }


  // Vec P -> B
  // B -> S \/ Vec P

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

  import scalaz._, Scalaz._

  val intsToText = Prism[String, Vector[Int]](
              _.split("[ ,]")
                .iterator
                .map(_.trim)
                .filter(_.nonEmpty)
                .map(is => \/.fromTryCatchNonFatal(is.toInt).toOption)
                .toVector
                .sequence
  )(_ mkString ", ")
}