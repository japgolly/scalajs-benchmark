package demo.suites.shootouts

import demo.Libraries
import demo.TempExt._
import demo.Util._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.callback.AsyncCallback
import japgolly.scalajs.react.vdom.html_<^._
import scala.scalajs.js
import scala.scalajs.js.|

object AsyncEffectShootout {

  type BM = Benchmark[Int]

  sealed abstract class Lib(val name: String) {
    def flatMap: BM
  }

  private def setup = Benchmark.setup[Int, List[Int => js.Promise[Int]]] { size =>
    val step = (s: Int) => new js.Promise[Int]((resolve, _) => {
      resolve(s + 1)
    })
    List.fill(size)(step)
  }

  // ===================================================================================================================

  object CatsEffect extends Lib(Libraries.CatsEffect.fullName) {
    import cats.effect._
    import cats.effect.unsafe.implicits.global

    private def toIO[A](f: => js.Promise[A]): IO[A] =
      IO.fromPromise(IO(f))

    override def flatMap = setup
      .map(_.iterator.foldLeft(IO.pure(0))((io, f) => io.flatMap(s => toIO(f(s)))))
      .async("")(_.unsafeToPromise())
  }

  // ===================================================================================================================

  object JsPromise extends Lib("JS Promise") {
    @inline private def toThenArg[A, B](f: A => js.Thenable[B]): js.Function1[A, B | js.Thenable[B]] =
      f(_)

    override def flatMap = setup
      .map(_.iterator.foldLeft(() => js.Promise.resolve[Int](0))((p, f) => () => p().`then`(toThenArg(f))))
      .async("")(_())
  }

  // ===================================================================================================================

  object ScalaJsReact extends Lib(Libraries.ScalaJsReact.fullName) {
    override def flatMap = setup
      .map(_.iterator.foldLeft(AsyncCallback.pure(0))((a, f) => a.flatMap(s => AsyncCallback.fromJsPromise(f(s)))))
      .async("")(_.unsafeToJsPromise())
  }

  // ===================================================================================================================

  final case class Params(lib: Lib, size: Int) {
    override def toString = s"${lib.name} @ $size"
  }

  val param1 = GuiParam.enumOf[Lib]("Library", CatsEffect, JsPromise, ScalaJsReact)(_.name)
  val param2 = GuiParam.int("Size", 500)

  val iso = GenIso.fields[Params]
  val params = GuiParams.combine2(iso)(param1, param2)

  val suite = Suite[Params]("Async effects")(
    Benchmark.derive("FlatMap", (_: Params).lib.flatMap)(_.size),
  )

  val guiSuite = GuiSuite(suite, params).describe(
    <.div(
      <.div("Shootout between async effect implementations."),
      linkToSource(sourceFilename)))
}
