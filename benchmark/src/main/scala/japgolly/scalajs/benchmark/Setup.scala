package japgolly.scalajs.benchmark

import japgolly.scalajs.react.AsyncCallback

/**
  * Given a `A`, set a `B` up and provide a [[Teardown]].
  */
final class Setup[-A, +B](val run: A => (B, Teardown)) extends AnyVal {
  def cmap[C](f: C => A): Setup[C, B] =
    new Setup(f andThen run)
}

object Setup {
  def empty[A]: Setup[A, A] =
    apply(identity)

  def unit[A](a: A): Setup[Unit, A] =
    new Setup(Function const ((a, Teardown.empty)))

  /** Setup only; no teardown. */
  def apply[A, B](f: A => B): Setup[A, B] =
    andTeardown(f, Teardown.empty)

  def andTeardown[A, B](f: A => B, t: Teardown): Setup[A, B] =
    new Setup(a => (f(a), t))

  def andTeardown[A, B](f: A => (B, Teardown)): Setup[A, B] =
    new Setup(f)

  def derive[A, B, C](f: A => Setup[B, C])(b: A => B): Setup[A, C] =
    new Setup(a => f(a) run b(a))
}

/**
  * Perform some effect to teardown something setup via [[Setup]].
  */
final class Teardown(val run: () => Unit) extends AnyVal {
  def asAsyncCallback: AsyncCallback[Unit] = AsyncCallback.point(run())
}

object Teardown {
  def apply(f: => Unit): Teardown =
    new Teardown(() => f)

  val empty: Teardown =
    new Teardown(() => ())
}