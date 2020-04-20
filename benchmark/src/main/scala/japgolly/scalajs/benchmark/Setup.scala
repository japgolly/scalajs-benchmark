package japgolly.scalajs.benchmark

import japgolly.scalajs.react.AsyncCallback

/**
  * Given a `A`, set a `B` up and provide a [[Teardown]].
  */
final case class Setup[-A, B](run: A => AsyncCallback[(B, Teardown)]) {
  def contramap[C](f: C => A): Setup[C, B] =
    new Setup(f andThen run)

  def map[C](f: B => C): Setup[A, C] =
    new Setup(run(_).map(x => (f(x._1), x._2)))

  def addTeardown(t: Teardown): Setup[A, B] =
    new Setup(run(_).map(x => (x._1, x._2 >> t)))

  def toBM[AA <: A]: Benchmark.Builder[AA, B] =
    new Benchmark.Builder(this)
}

object Setup {
  def passthrough[A]: Setup[A, A] =
    simple(identity)

  def const[A](a: AsyncCallback[(A, Teardown)]): Setup[Unit, A] =
    new Setup(_ => a)

  def pure[A](a: A): Setup[Unit, A] =
    const(AsyncCallback.pure((a, Teardown.empty)))

  /** Setup only; no teardown. */
  def simple[A, B](f: A => B): Setup[A, B] =
    new Setup[A, B](a => AsyncCallback.point((f(a), Teardown.empty)))

  def async[A, B](f: A => AsyncCallback[B]): Setup[A, B] =
    new Setup[A, B](f(_).map((_, Teardown.empty)))

  def derive[A, B, C](f: A => Setup[B, C])(b: A => B): Setup[A, C] =
    new Setup(a => f(a) run b(a))
}

/**
  * Perform some effect to teardown something setup via [[Setup]].
  */
final case class Teardown(asAsyncCallback: AsyncCallback[Unit]) {
  def >>(next: Teardown): Teardown =
    Teardown(asAsyncCallback >> next.asAsyncCallback)

  @inline def <<(prev: Teardown): Teardown =
    prev >> this
}

object Teardown {
  val empty: Teardown =
    apply(AsyncCallback.unit)
}