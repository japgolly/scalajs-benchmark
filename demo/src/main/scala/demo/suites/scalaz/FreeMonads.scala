package demo.suites.scalaz

import demo.Util._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.vdom.html_<^._
import scalaz.effect.IO
import scalaz.{Free, Kleisli, ReaderT, ~>}

import scalaz.std.function.function0Instance

object FreeMonads {

  /**
    * The target implementation into which `Cmd` algebras below are translated.
    */
  class TheRealDeal {
    private var ai = 0
    def add(b: Int): Unit = ai += b
    def get(): Int = ai
  }

  trait Cmd[A]
  case class Add(b: Int) extends Cmd[Unit]
  case object Get extends Cmd[Int]

  type FreeCmd[A] = Free[Cmd, A]
  implicit def autoLiftCmd[A](c: Cmd[A]): FreeCmd[A] = Free.liftF(c)

  type ReaderIO[A] = ReaderT[IO, TheRealDeal, A]
  implicit object CmdToReaderIO extends (Cmd ~> ReaderIO) {
    def io[A](f: TheRealDeal => A) = Kleisli((rd: TheRealDeal) => IO{ f(rd) })
    override def apply[A](ta: Cmd[A]): ReaderIO[A] = ta match {
      case Add(n) => io{ _.add(n) }
      case Get    => io{ _.get }
    }
  }

  type ReaderF[A] = ReaderT[Function0, TheRealDeal, A]
  implicit object CmdToReaderF extends (Cmd ~> ReaderF) {
    def io[A](f: TheRealDeal => A) = Kleisli((rd: TheRealDeal) => () => f(rd))
    override def apply[A](ta: Cmd[A]): ReaderF[A] = ta match {
      case Add(n) => io{ _.add(n) }
      case Get    => io{ _.get }
    }
  }

  def CmdToFn0(rd: TheRealDeal): Cmd ~> Function0 = new (Cmd ~> Function0) {
    override def apply[A](m: Cmd[A]): () => A = m match {
      case Add(n) => () => rd.add(n)
      case Get    => () => rd.get
    }
  }

  def CmdToIO(rd: TheRealDeal): Cmd ~> IO = new (Cmd ~> IO) {
    override def apply[A](m: Cmd[A]): IO[A] = m match {
      case Add(n) => IO{ rd.add(n) }
      case Get    => IO{ rd.get }
    }
  }

  val makeRealFn0 = CmdToFn0(new TheRealDeal)
  val makeRealIO = CmdToIO(new TheRealDeal)

  val bm = Benchmark.setup[Int, FreeCmd[Int]] { size =>
    val add1: FreeCmd[Unit] = Add(1)
    val addn = Iterator.continually(add1)
      .take(size - 2) // -1 for the foldLeft(z), -1 for the final bind to Get
      .foldLeft(add1)((a, b) => a >>= (_ => b))
    addn >>= (_ => Get)
  }

  val prefix = "Free → "

  val bmFn0FoldMap =
    bm(prefix + "Fn0 (foldMap)"){ p1 =>
      val p2: Function0[Int] = p1.foldMap(makeRealFn0)
      p2(): Int
    }

  val bmFn0MapSuspension =
    bm(prefix + "Fn0 (mapSuspension)"){ p1 =>
      val p2: Free[Function0, Int] = p1.mapSuspension(makeRealFn0)
      p2.run: Int
    }

  val bms = Vector[Benchmark[Int]](
    bmFn0FoldMap,
    bmFn0MapSuspension,

    bm(prefix + "Reader[Fn0]"){ p1 =>
      val p2: ReaderF[Int] = p1.foldMap(CmdToReaderF)
      p2.run(new TheRealDeal)(): Int
    },

    bm(prefix + "IO"){ p1 =>
      val p2: IO[Int] = p1.foldMap(makeRealIO)
      p2.unsafePerformIO(): Int
    },

    bm(prefix + "Reader[IO]"){ p1 =>
      val p2: ReaderIO[Int] = p1.foldMap(CmdToReaderIO)
      p2.run(new TheRealDeal).unsafePerformIO(): Int
    }
  )

  //======================================================================================================================

  val suite = Suite("Free monads")(bms: _*)

  val param = GuiParam.int("Size", 50, 500)

  val guiSuite = GuiSuite(suite, param).describe(
    <.div(
      <.div(
        "There are some experiments with ", <.code("Free"), " that I wrote in late 2014."),
      <.div(^.marginTop := "1ex",
        "There are newer constructs available (like \"freer\" monads now?) that would benefit from being included and likely will be in the near future."),
      <.div(^.marginTop := "1ex", ^.fontStyle.italic,
        "Note: ", <.code("Task"), " is excluded because it doesn't work on Scala.JS."),
      linkToSource(sourceFilename))
  )
}
