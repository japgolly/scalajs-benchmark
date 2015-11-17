package whatever

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
  type Editor[S] = ExternalVar[Option[S]] => ReactElement


  type Key[T] = monocle.Lens[GenState, Option[T]]
//  trait Key[T] {
//    def get(s: GenState): Option[T]
//    def mod(s: GenState)(f: Option[T] => Option[T]): GenState
//  }
  object Key {
    private var global = ' '

    def apply[T](): Key[T] = {
      global = (global + 1).toChar
      val key = global

//      new Key[T] {
//        override def get(s: GenState): Option[T] = {
//          val o: Option[Any] = s.flatMap(_.get(key))
//          o.asInstanceOf[Option[T]]
//        }
//      }

      monocle.Lens[GenState, Option[T]](
        s => {
          val o: Option[Any] = s.get(key)
          o.asInstanceOf[Option[T]]
        }
      )({
        case Some(t) => _.updated(key, t)
        case None    => _ - key
      })
    }
  }

//  type PDState = Map[Char, Any]
//  type GenState = Option[PDState]

  type GenState = Map[Char, Any]

  case class Param[A, B](header: Header, renderValue: RenderValue[A], editor: Editor[B], initState: Option[B])

  trait ParamWithKey[A] {
    type B
    val param: Param[A, B]
    val key: Key[B]

    def editor(e: ExternalVar[GenState]): TagMod =
      param editor ExternalVar(key get e.value)(ob => e.set(key.set(ob)(e.value)))

    def parseEditorState(ob: Option[B]): String \/ Vector[A]
  }

  trait Params[P] {
    val paramDefs: Vector[ParamWithKey[P]]
    val forState: GenState => ParamWithKey[P] \/ Vector[P]

    def initState: GenState =
      paramDefs.foldLeft(Map.empty: GenState)((m, p) =>
        p.key.set(p.param.initState)(m))

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
        ^.value := e.value.getOrElse(""),
        ^.onChange ==> ((i: ReactEventI) => e.set(Some(i.target.value).filter(_.nonEmpty))))
}