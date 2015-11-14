package whatever

//import java.util.concurrent.TimeUnit._
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

object Benchy {

  case class Suite[I](name: String,
                      bms: Vector[Benchmark[I]],
                      params: Vector[I])

  type BenchmarkFn = () => Any
  type SetupFn[-I] = I => BenchmarkFn

  case class Benchmark[-I](name: String, setup: SetupFn[I]) {
    override def toString = name
  }


//  type ValuesForParam[A] = Vector[A]
//
//  case class Param[A](name: String, default: Option[ValuesForParam[A]])

//  trait Params[+A] {
//    def produce: Vector[A]
//  }

//  trait ParamsAndValues[+A] {
//  }


  // ====================================================================================================

  case class RunStats(times: Vector[FiniteDuration]) {
    override def toString() = {
      val opsPerSec = 1.second / average
//      val opsPerSec = "%0.3f".format(1.second / average)
      s"$opsPerSec ops/sec ($runs runs, Σ $totalTime)"
    }

    def runs =
      times.length

    def totalTime: FiniteDuration =
      if (times.isEmpty)
        Duration.Zero
      else
        times.reduce(_ + _)

    def average: Duration =
      if (times.isEmpty)
        Duration.Inf
      else
        totalTime / runs
  }
  type RunResult = Either[Throwable, RunStats]
  case class Result[A](bm: Benchmark[A], params: A, result: RunResult)
  def runSuite[A](s: Suite[A]): Unit = {
    val clock = Clock.Default

    for {
      p <- s.params
      b <- s.bms
    } {

      val fn = b.setup(p)
      val rr: RunResult =
        try {

          @tailrec
          def go(rs1: RunStats): RunStats = {
            val t = clock.justTime(fn())
//            val t = clock.justTime((1 to 10).foreach(_ => fn()))
            val rs2 = RunStats(rs1.times :+ t)
            if (enough_?(rs2))
              rs2
            else
              go(rs2)
          }

          Right(go(RunStats(Vector.empty)))
        } catch {
          case t: Throwable => Left(t)
        }

      println(Result(b, p, rr))
    }

  }

  def enough_?(s: RunStats): Boolean = {
    def small = s.runs >= 10000
    def large = s.totalTime > 3.seconds
//    (s.totalTime > 5.second) || (s.runs >= 1000)
    small || large
  }

  trait Clock {
    type Time
    def justTime(f: => Any): FiniteDuration
  }

  trait StatelessClock extends Clock {
    def get: Time
    def duration(start: Time, end: Time): FiniteDuration

    val bh = new Blackhole

    override def justTime(f: => Any): FiniteDuration = {
      val a = get
      val x = f
      val b = get
//      bh.consumeA(x)
      duration(a, b)
    }
  }

  object Clock {
    val Default = SysMilli

    object SysNano extends StatelessClock {
      override type Time                      = Long
      override def get                        = System.nanoTime()
      override def duration(a: Time, b: Time) = FiniteDuration(b - a, NANOSECONDS)
    }

    object SysMilli extends StatelessClock {
      override type Time                      = Long
      override def get                        = System.currentTimeMillis()
      override def duration(a: Time, b: Time) = FiniteDuration(b - a, MILLISECONDS)
    }

//    object Chrome extends Clock {
//
//      @JSName("chrome.Interval")
//      private class Interval() extends js.Any
//
//      private val i = new Interval().asInstanceOf[js.Dynamic]
//
//      override type Time                      = Double
//      override def get                        =
//      override def duration(a: Time, b: Time) = FiniteDuration(b - a, MILLISECONDS)
//    }

  }
}
