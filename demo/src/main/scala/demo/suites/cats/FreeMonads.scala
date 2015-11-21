package demo.suites.cats

import demo.Util._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._

import cats.{~>, Functor}
import cats.data.{Kleisli, ReaderT}
import cats.free.Free
//import cats.effect.IO
import cats.std.function.function0Instance

object FreeMonads {

  /**
    * The target implementation into which `Cmd` algebras below are translated.
    */
  class TheRealDeal {
    private var ai = 0
    def add(b: Int): Unit = ai += b
    def get(): Int = ai
  }

  //======================================================================================================================

  object FreeK {
    trait Cmd[Next]
    case class Add[Next](b: Int, k: () => Next) extends Cmd[Next]
    case class Get[Next](k: Int => Next) extends Cmd[Next]

    implicit object CmdFunctor extends Functor[Cmd] {
      override def map[A, B](fa: Cmd[A])(f: A => B): Cmd[B] = fa match {
        case Add(b, k) => Add(b, () => f(k()))
        case Get(k)    => Get(f compose k)
      }
    }

    type FreeCmd[A] = Free[Cmd, A]
    def addCmd(b: Int): FreeCmd[Unit] = Free.liftF(Add(b, () => ()))
    val getCmd: FreeCmd[Int] = Free.liftF(Get(identity))

    type ReaderF[A] = ReaderT[Function0, TheRealDeal, A]
    implicit object CmdToReaderF extends (Cmd ~> ReaderF) {
      def io[A](f: TheRealDeal => A) = Kleisli((rd: TheRealDeal) => () => f(rd))
      override def apply[A](ta: Cmd[A]): ReaderF[A] = ta match {
        case Add(n, k) => io{ rd => rd.add(n); k() }
        case Get(k)    => io{ rd => k(rd.get) }
      }
    }

    def CmdToFn0(rd: TheRealDeal): Cmd ~> Function0 = new (Cmd ~> Function0) {
      override def apply[A](m: Cmd[A]): () => A = m match {
        case Add(n, k) => () => { rd.add(n); k() }
        case Get(k)    => () => { k(rd.get) }
      }
    }

    val bm = Benchmark.setup[Int, FreeCmd[Int]] { size =>
      val add1: FreeCmd[Unit] = addCmd(1)
      val addn = Iterator.continually(add1)
        .take(size - 2) // -1 for the foldLeft(z), -1 for the final bind to Get
        .foldLeft(add1)((a, b) => a flatMap (_ => b))
      addn flatMap (_ => getCmd)
    }

    val prefix = "Free → "
    val bms = Vector[Benchmark[Int]](

      bm(prefix + "Fn0 (mapSuspension)"){ p1 =>
        val p2: Free[Function0, Int] = p1.mapSuspension(CmdToFn0(new TheRealDeal))
        val r: Int = p2.run
      },

      bm(prefix + "Fn0 (foldMap)"){ p1 =>
        val p2: Function0[Int] = p1.foldMap(CmdToFn0(new TheRealDeal))
        val r: Int = p2()
      },

      bm(prefix + "Reader[Fn0]"){ p1 =>
        val p2: ReaderF[Int] = p1.foldMap(CmdToReaderF)
        val r: Int = p2.run(new TheRealDeal)()
      }
    )
  }

  //======================================================================================================================

  object Coyo {
    import cats.free.Coyoneda

    type CoyonedaF[F[_]] = ({type A[α] = Coyoneda[F, α]})
    type FreeC[S[_], A] = Free[({type f[x] = Coyoneda[S, x]})#f, A]
    def liftFC[S[_], A](s: S[A]): FreeC[S, A] = Free.liftF[CoyonedaF[S]#A, A](Coyoneda lift s)

    /** Turns a natural transformation F ~> G into CF ~> CG */
    def liftT[F[_], G[_]](fg: F ~> G): CoyonedaF[F]#A ~> CoyonedaF[G]#A =
      new (CoyonedaF[F]#A ~> CoyonedaF[G]#A) {
        def apply[A](c: Coyoneda[F, A]) = c.transform(fg)
      }

    /** Turns a natural transformation F ~> G into CF ~> G */
    def liftTF[F[_], G[_]: Functor](fg: F ~> G): CoyonedaF[F]#A ~> G = {
      type CF[A] = Coyoneda[F, A]
      type CG[A] = Coyoneda[G, A]
      val m: (CF ~> CG) = liftT(fg)
      val n: (CG ~> G) = new (CG ~> G) {
        def apply[A](fa: CG[A]): G[A] = fa.run
      }
      n compose m
    }

    trait Cmd[A]
    case class Add(b: Int) extends Cmd[Unit]
    case object Get extends Cmd[Int]

    type FreeCmd[A] = FreeC[Cmd, A]
    implicit def autoLiftCmd[A](c: Cmd[A]) = liftFC(c)

    type ReaderF[A] = ReaderT[Function0, TheRealDeal, A]
    implicit object CmdToReaderF extends (Cmd ~> ReaderF) {
      def io[A](f: TheRealDeal => A) = Kleisli((rd: TheRealDeal) => () => f(rd))
      override def apply[A](ta: Cmd[A]): ReaderF[A] = ta match {
        case Add(n) => io{ _.add(n) }
        case Get    => io{ _.get }
      }
    }
    val CmdToReaderF_ = liftTF(CmdToReaderF)

    def CmdToFn0(rd: TheRealDeal): Cmd ~> Function0 = new (Cmd ~> Function0) {
      override def apply[A](m: Cmd[A]): () => A = m match {
        case Add(n) => () => rd.add(n)
        case Get    => () => rd.get
      }
    }

    val bm = Benchmark.setup[Int, FreeCmd[Int]] { size =>
      val add1: FreeCmd[Unit] = Add(1)
      val addn = Iterator.continually(add1)
        .take(size - 2) // -1 for the foldLeft(z), -1 for the final bind to Get
        .foldLeft(add1)((a, b) => a flatMap (_ => b))
      addn flatMap (_ => Get)
    }

    val prefix = "Free & CoYoneda → "
    val bms = Vector[Benchmark[Int]](

      bm(prefix + "Fn0 (foldMap)"){ p1 =>
        val nt = liftTF(CmdToFn0(new TheRealDeal))
        val p2: Function0[Int] = p1.foldMap(nt)
        val r: Int = p2()
      },

      bm(prefix + "Fn0 (mapSuspension)"){ p1 =>
        val nt = liftTF(CmdToFn0(new TheRealDeal))
        val p2: Free[Function0, Int] = p1.mapSuspension(nt)
        val r: Int = p2.run
      },

      bm(prefix + "Reader[Fn0]"){ p1 =>
        val p2: ReaderF[Int] = p1.foldMap(CmdToReaderF_)
        val r: Int = p2.run(new TheRealDeal)()
      }
    )
  }

  //======================================================================================================================

  val suite = Suite("Free Monads")(FreeK.bms ++ Coyo.bms: _*)

  val param = GuiParam(Render.int, Editor.text, Parser.intsAsText)("Size", 50, 500)

  val guiSuite = GuiSuite(suite, param).describe(
    <.div(
      <.div(
        "There are some experiments with ", <.code("Free"), " that I wrote in late 2014 with Scalaz, ported here to Cats."),
      <.div(^.marginTop := "1ex",
        "There are newer constructs available (like \"freer\" monads now?) that would benefit from being included and likely will be in the near future."),
      linkToSource(sourceFilename))
  )
}