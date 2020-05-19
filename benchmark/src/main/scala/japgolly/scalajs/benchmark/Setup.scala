package japgolly.scalajs.benchmark

import japgolly.scalajs.react.{AsyncCallback, CallbackTo}

final case class SetupCtx(aborted: CallbackTo[Boolean]) {

  private val throwCosAborted: AsyncCallback[Unit] =
    AsyncCallback.throwException(new RuntimeException("Aborted."))

  val throwWhenAborted: AsyncCallback[Unit] =
    aborted.asAsyncCallback.flatMap(throwCosAborted.when_(_))
}

/**
  * Given a `A`, set a `B` up and provide a [[Teardown]].
  */
final case class Setup[-A, B](run: (A, SetupCtx) => AsyncCallback[(B, Teardown)]) {

  def contramap[C](f: C => A): Setup[C, B] =
    contramapAsync(c => AsyncCallback.delay(f(c)))

  def contramapAsync[AA <: A, C](f: C => AsyncCallback[AA]): Setup[C, B] =
    Setup.async(f) >>> this

  def map[C](f: B => C): Setup[A, C] =
    mapAsync(b => AsyncCallback.delay(f(b)))

  def flatMap[AA <: A, C](next: B => Setup[Unit, C]): Setup[AA, C] =
    new Setup((a, s) =>
      for {
        (b, t1) <- run(a, s)
        _       <- s.throwWhenAborted
        (c, t2) <- next(b).run((), s)
      } yield (c, t1 >> t2)
    )

  def mapAsync[C](f: B => AsyncCallback[C]): Setup[A, C] =
    this >>> Setup.async(f)

  def addTeardown(t: Teardown): Setup[A, B] =
    new Setup(run(_, _).map(x => (x._1, x._2 >> t)))

  def toBM[AA <: A]: Benchmark.Builder[AA, B] =
    new Benchmark.Builder(this)

  def >>>[AA <: A, C](next: Setup[B, C]): Setup[AA, C] =
    new Setup((a, s) =>
      for {
        (b, t1) <- run(a, s)
        _       <- s.throwWhenAborted
        (c, t2) <- next.run(b, s)
      } yield (c, t1 >> t2)
    )

  def tap[AA <: A](f: B => Any): Setup[AA, B] =
    this >>> Setup.simple { b => f(b); b }

  def tapAsync[AA <: A](f: B => AsyncCallback[Any]): Setup[AA, B] =
    this >>> Setup.async(b => f(b).ret(b))

  def tapSync[AA <: A](f: B => CallbackTo[Any]): Setup[AA, B] =
    tapAsync(f(_).asAsyncCallback)
}

object Setup {
  def passthrough[A]: Setup[A, A] =
    simple(identity)

  def const[A](a: AsyncCallback[(A, Teardown)]): Setup[Unit, A] =
    new Setup((_, _) => a)

  def pure[A](a: A): Setup[Unit, A] =
    const(AsyncCallback.pure((a, Teardown.empty)))

  /** Setup only; no teardown. */
  def simple[A, B](f: A => B): Setup[A, B] =
    new Setup[A, B]((a, _) => AsyncCallback.delay((f(a), Teardown.empty)))

  def async[A, B](f: A => AsyncCallback[B]): Setup[A, B] =
    new Setup[A, B]((a, _) => f(a).map((_, Teardown.empty)))

  def derive[A, B, C](f: A => Setup[B, C])(b: A => B): Setup[A, C] =
    new Setup((a, s) => f(a).run(b(a), s))
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