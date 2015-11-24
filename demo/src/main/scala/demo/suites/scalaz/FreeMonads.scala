package demo.suites.scalaz

import demo.Util._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._

import scalaz.{Free, ~>, Functor, ReaderT, Kleisli}
import scalaz.concurrent.Task
import scalaz.effect.IO
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

    type ReaderIO[A] = ReaderT[IO, TheRealDeal, A]
    implicit object CmdToReaderIO extends (Cmd ~> ReaderIO) {
      def io[A](f: TheRealDeal => A) = Kleisli((rd: TheRealDeal) => IO{ f(rd) })
      override def apply[A](ta: Cmd[A]): ReaderIO[A] = ta match {
        case Add(n, k) => io{ rd => rd.add(n); k() }
        case Get(k)    => io{ rd => k(rd.get) }
      }
    }

    type ReaderTask[A] = ReaderT[Task, TheRealDeal, A]
    implicit object CmdToReaderTask extends (Cmd ~> ReaderTask) {
      def io[A](f: TheRealDeal => A) = Kleisli((rd: TheRealDeal) => Task.delay{ f(rd) })
      override def apply[A](ta: Cmd[A]): ReaderTask[A] = ta match {
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

    def CmdToIO(rd: TheRealDeal): Cmd ~> IO = new (Cmd ~> IO) {
      override def apply[A](m: Cmd[A]): IO[A] = m match {
        case Add(n, k) => IO{ rd.add(n); k() }
        case Get(k)    => IO{ k(rd.get) }
      }
    }

    def CmdToTask(rd: TheRealDeal): Cmd ~> Task = new (Cmd ~> Task) {
      override def apply[A](m: Cmd[A]): Task[A] = m match {
        case Add(n, k) => Task.delay{ rd.add(n); k() }
        case Get(k)    => Task.delay{ k(rd.get) }
      }
    }

    val bm = Benchmark.setup[Int, FreeCmd[Int]] { size =>
      val add1: FreeCmd[Unit] = addCmd(1)
      val addn = Iterator.continually(add1)
        .take(size - 2) // -1 for the foldLeft(z), -1 for the final bind to Get
        .foldLeft(add1)((a, b) => a >>= (_ => b))
      addn >>= (_ => getCmd)
    }

    val prefix = "Free → "

    val bmFn0FoldMap =
      bm(prefix + "Fn0 (foldMap)"){ p1 =>
        val p2: Function0[Int] = p1.foldMap(CmdToFn0(new TheRealDeal))
        val r: Int = p2()
      }

    val bms = Vector[Benchmark[Int]](
      bmFn0FoldMap,

      bm(prefix + "Fn0 (mapSuspension)"){ p1 =>
        val p2: Free[Function0, Int] = p1.mapSuspension(CmdToFn0(new TheRealDeal))
        val r: Int = p2.run
      },

      bm(prefix + "Reader[Fn0]"){ p1 =>
        val p2: ReaderF[Int] = p1.foldMap(CmdToReaderF)
        val r: Int = p2.run(new TheRealDeal)()
      },

//      bm(prefix + "Task"){ p1 =>
//        val p2: Task[Int] = p1.foldMap(CmdToTask(new TheRealDeal))
//        val r: Int = p2.unsafePerformTask()
//      },
//
//      bm(prefix + "Reader[Task]"){ p1 =>
//        val p2: ReaderTask[Int] = p1.foldMap(CmdToReaderTask)
//        val r: Int = p2.run(new TheRealDeal).unsafePerformTask()
//      },

      bm(prefix + "IO"){ p1 =>
        val p2: IO[Int] = p1.foldMap(CmdToIO(new TheRealDeal))
        val r: Int = p2.unsafePerformIO()
      },

      bm(prefix + "Reader[IO]"){ p1 =>
        val p2: ReaderIO[Int] = p1.foldMap(CmdToReaderIO)
        val r: Int = p2.run(new TheRealDeal).unsafePerformIO()
      }
    )
  }

  //======================================================================================================================

  object Coyo {
    import scalaz.Coyoneda.liftTF
    import Free.{liftFC, FreeC}

    trait Cmd[A]
    case class Add(b: Int) extends Cmd[Unit]
    case object Get extends Cmd[Int]

    type FreeCmd[A] = FreeC[Cmd, A]
    implicit def autoLiftCmd[A](c: Cmd[A]) = liftFC(c)

    type ReaderIO[A] = ReaderT[IO, TheRealDeal, A]
    implicit object CmdToReaderIO extends (Cmd ~> ReaderIO) {
      def io[A](f: TheRealDeal => A) = Kleisli((rd: TheRealDeal) => IO{ f(rd) })
      override def apply[A](ta: Cmd[A]): ReaderIO[A] = ta match {
        case Add(n) => io{ _.add(n) }
        case Get    => io{ _.get }
      }
    }
    val CmdToReaderIO_ = liftTF(CmdToReaderIO)

    type ReaderTask[A] = ReaderT[Task, TheRealDeal, A]
    implicit object CmdToReaderTask extends (Cmd ~> ReaderTask) {
      def io[A](f: TheRealDeal => A) = Kleisli((rd: TheRealDeal) => Task.delay{ f(rd) })
      override def apply[A](ta: Cmd[A]): ReaderTask[A] = ta match {
        case Add(n) => io{ _.add(n) }
        case Get    => io{ _.get }
      }
    }
    val CmdToReaderTask_ = liftTF(CmdToReaderTask)

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

    def CmdToIO(rd: TheRealDeal): Cmd ~> IO = new (Cmd ~> IO) {
      override def apply[A](m: Cmd[A]): IO[A] = m match {
        case Add(n) => IO{ rd.add(n) }
        case Get    => IO{ rd.get }
      }
    }

    def CmdToTask(rd: TheRealDeal): Cmd ~> Task = new (Cmd ~> Task) {
      override def apply[A](m: Cmd[A]): Task[A] = m match {
        case Add(n) => Task.delay{ rd.add(n) }
        case Get    => Task.delay{ rd.get }
      }
    }

    val bm = Benchmark.setup[Int, FreeCmd[Int]] { size =>
      val add1: FreeCmd[Unit] = Add(1)
      val addn = Iterator.continually(add1)
        .take(size - 2) // -1 for the foldLeft(z), -1 for the final bind to Get
        .foldLeft(add1)((a, b) => a >>= (_ => b))
      addn >>= (_ => Get)
    }

    val prefix = "Free & coYoneda → "

    val bmFn0FoldMap =
      bm(prefix + "Fn0 (foldMap)"){ p1 =>
        val nt = liftTF(CmdToFn0(new TheRealDeal))
        val p2: Function0[Int] = p1.foldMap(nt)
        val r: Int = p2()
      }

    val bms = Vector[Benchmark[Int]](
      bmFn0FoldMap,

      bm(prefix + "Fn0 (mapSuspension)"){ p1 =>
        val nt = liftTF(CmdToFn0(new TheRealDeal))
        val p2: Free[Function0, Int] = p1.mapSuspension(nt)
        val r: Int = p2.run
      },

      bm(prefix + "Reader[Fn0]"){ p1 =>
        val p2: ReaderF[Int] = p1.foldMap(CmdToReaderF_)
        val r: Int = p2.run(new TheRealDeal)()
      },

//      bm(prefix + "Task"){ p1 =>
//        val nt = liftTF(CmdToTask(new TheRealDeal))
//        val p2: Task[Int] = p1.foldMap(nt)
//        val r: Int = p2.run
//      },

//      bm(prefix + "Reader[Task]"){ p1 =>
//        val p2: ReaderTask[Int] = p1.foldMap(CmdToReaderTask_)
//        val r: Int = p2.run(new TheRealDeal).run
//      },

      bm(prefix + "IO"){ p1 =>
        val nt = liftTF(CmdToIO(new TheRealDeal))
        val p2: IO[Int] = p1.foldMap(nt)
        val r: Int = p2.unsafePerformIO()
      },

      bm(prefix + "Reader[IO]"){ p1 =>
        val p2: ReaderIO[Int] = p1.foldMap(CmdToReaderIO_)
        val r: Int = p2.run(new TheRealDeal).unsafePerformIO()
      }
    )
  }

  //======================================================================================================================

  val suite = Suite("Free monads")(FreeK.bms ++ Coyo.bms: _*)

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