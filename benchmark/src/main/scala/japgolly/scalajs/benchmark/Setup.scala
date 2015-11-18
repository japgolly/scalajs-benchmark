package japgolly.scalajs.benchmark

/**
  * Given a `A`, set a `B` up and provide a [[Teardown]].
  */
final class Setup[-A, +B](val run: A => (B, Teardown)) extends AnyVal

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
}

/**
  * Perform some effect to teardown something setup via [[Setup]].
  */
final class Teardown(val run: () => Unit) extends AnyVal

object Teardown {
  def apply(f: => Unit): Teardown =
    new Teardown(() => f)

  val empty: Teardown =
    Teardown(() => ())
}