package whatever

//import java.util.concurrent.TimeUnit._
import japgolly.scalajs.react.Callback

import org.scalajs.dom
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSName
import scala.util.Try

object Benchy {

  case class Suite[I](name: String,
                      bms: Vector[Benchmark[I]],
                      params: Vector[I]) {
    val totalBenchmarks: Int =
      bms.length * params.length
  }

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

  sealed trait Event[A]
  case class SuiteStarting[A](p: Progress[A]) extends Event[A]
  case class BenchmarkStarting[A](p: Progress[A], bm: Benchmark[A], param: A) extends Event[A]
  case class BenchmarkFinished[A](p: Progress[A], bm: Benchmark[A], param: A, result: RunResult) extends Event[A]
  case class SuiteFinished[A](p: Progress[A] /*, aborted | results, */) extends Event[A]

  case class AbortFn(run: () => Unit)

  def runToConsole[A](s: Suite[A], delay: FiniteDuration = DefaultDelay): Unit = {
    val fmt = {
      val prog  = s.totalBenchmarks.toString.length
      val name  = s.bms.foldLeft(0)(_ max _.name.length)
      var param = s.params.foldLeft(0)(_ max _.toString.length)
      if (!s.params.forall(_.toString.matches("^-?\\d+$")))
        param = -param
      s"[%${prog}d/%d] %-${name}s %${param}s : %s"
    }

    runSuiteAsync(s, delay) {
      case SuiteStarting    (p)           => println("Starting...")
      case BenchmarkStarting(p, bm, a)    => ()
      case BenchmarkFinished(p, bm, a, r) => println(fmt.format(p.run, p.total, bm.name, a, r))
      case SuiteFinished    (p)           => println("Done.")
    }
  }

  def runSuiteAsync[A](s: Suite[A], delay: FiniteDuration = DefaultDelay)(eh: Event[A] => Unit): AbortFn = {
    val clock = Clock.Default

    def run: Unit = {
      var progress = Progress(s, 0)

      eh(SuiteStarting(progress))

      for {
        p <- s.params
        b <- s.bms
      } {

        eh(BenchmarkStarting(progress, b, p))

        val fn = b.setup(p)

        def runBM(): RunResult =
          try {
            val rs = new RunStatsM
            @tailrec
            def go: Unit = {
              val t = clock.justTime(fn())
              rs add t
              if (!enough_?(rs))
                go
            }
            go
            Right(RunStats(rs.times.toVector))
          } catch {
            case t: Throwable =>
//              t.printStackTrace()
              Left(t)
            //              throw t
          }

        // warmup first
//        val rr = runBM().right.flatMap(_ => {
//          runBM()
//        })

        val rr = runBM()

        progress = progress.copy(run = progress.run + 1)
        eh(BenchmarkFinished(progress, b, p, rr))
      }

      eh(SuiteFinished(progress))

    }

    val hnd = js.timers.setTimeout(delay)(run)
    AbortFn(() => js.timers.clearTimeout(hnd))
  }

  val DefaultDelay: FiniteDuration = 4.millis

  // suites, benchmarks, and run fns should all take options with shit like the enoughFn, clock

  // ====================================================================================================


  case class Progress[A](s: Suite[A], run: Int) {
    def total = s.totalBenchmarks
    def remaining = total - run
  }

  def toOpsPerSec(d: FiniteDuration): Double =
    d.toMicros.toDouble / 1000000L.toDouble

  def trimOutliers(v: Vector[Double]): Vector[Double] = {
    val t = (v.length.toDouble * 0.01).toInt
    if (t > 0) {
//      println(s"Dropping: ${v.sorted.takeRight(t).reverse}")
      v.sorted.dropRight(t)
    }
    else
      v
  }

  case class RunStats(times: Vector[FiniteDuration]) {
    override def toString() = {
      val tot = "%0.3f sec".format(toOpsPerSec(totalTime))

//      val opsPerSec = 1.second / average
////      val opsPerSec = "%0.3f".format(1.second / average)
//      val moe = toOpsPerSec(marginOfError)
//      s"$opsPerSec ± $moe ops/sec ($runs runs, Σ $tot)"

      def fmtD(d: Duration): String =
        //"%0.3fms" format (d.toMicros / 1000.0)
        d.toMicros.toInt + "μs"

//      s"${fmtD(average)} ± ${fmtD(marginOfError)} ${"%0.1f%%" format statsInMicroSec.rme} /op ($runs runs, Σ $tot)"
      s"${fmtD(average)} ± ${fmtD(marginOfError)} ${statsInMicroSec.rme.toInt}% /op ($runs runs, Σ $tot)"
    }

    def runs =
      times.length

    val totalTime: FiniteDuration =
      if (times.isEmpty)
        Duration.Zero
      else
        times.reduce(_ + _)

    val average: Duration =
      if (times.isEmpty)
        Duration.Inf
      else
        totalTime / runs

    val statsInMicroSec: Stats =
      Stats(trimOutliers(times.map(_.toMicros.toDouble)))

    def marginOfError: FiniteDuration =
      FiniteDuration(statsInMicroSec.marginOfError.toLong, MICROSECONDS)
  }

  class RunStatsM {
    var times = new js.Array[FiniteDuration]
    var totalTime: FiniteDuration = Duration.Zero

    def add(d: FiniteDuration): Unit = {
      times push d
      totalTime += d
    }

    def runs = times.length
  }

//  type Enough_? = RunStatsM => Boolean

  type RunResult = Either[Throwable, RunStats]
//  case class Result[A](bm: Benchmark[A], params: A, result: RunResult)

  // ====================================================================================================

//  val minRuns = 1000
//  val minTime = 1.second
//  val maxTime = 2.second

  val minRuns = 10000
  val minTime = 3.second
  val maxTime = 30.second

  def enough_?(s: RunStatsM): Boolean = {
    def small = s.runs >= minRuns && s.totalTime >= minTime
    def large = s.totalTime > maxTime
    small || large
  }

  // ====================================================================================================

  trait Clock {
    def justTime(f: => Any): FiniteDuration
  }

  trait StatelessClock extends Clock {
    type Time
    def get: Time
    def duration(start: Time, end: Time): FiniteDuration

    val bh = new Blackhole

    override def justTime(f: => Any): FiniteDuration = {
      val a = get
      val x = f
      val b = get
      // bh.consumeA(x)
      duration(a, b)
    }
  }

  object Clock {
    object SysNano extends StatelessClock {
      override def toString = "SysNano"
      override type Time                      = Long
      override def get                        = System.nanoTime()
      override def duration(a: Time, b: Time) = FiniteDuration(b - a, NANOSECONDS)
    }

    object SysMilli extends StatelessClock {
      override def toString = "SysMilli"
      override type Time                      = Long
      override def get                        = System.currentTimeMillis()
      override def duration(a: Time, b: Time) = FiniteDuration(b - a, MILLISECONDS)
    }

    @JSName("chrome.Interval")
    @js.native
    class ChromeInterval() extends js.Any {
      def start(): Unit = js.native
      def stop(): Unit = js.native
      def microseconds(): Double = js.native
    }

    val Chrome: Option[Clock] =
      Try(new ChromeInterval()).toOption.map(i =>
        new Clock {
          override def toString = "Chrome"
          override def justTime(f: => Any): FiniteDuration = {
            i.start()
            val x = f
            i.stop()
            // bh.consumeA(x)
            FiniteDuration(i.microseconds().toLong, MICROSECONDS)
          }
        }
      )

    /*
    From https://github.com/bestiejs/benchmark.js/blob/master/benchmark.js

      // Detect Node.js's nanosecond resolution timer available in Node.js >= 0.8.
      if (processObject && typeof (timer.ns = processObject.hrtime) == 'function') {
        timers.push({ 'ns': timer.ns, 'res': getRes('ns'), 'unit': 'ns' });
      }
      // Detect Wade Simmons' Node.js `microtime` module.
      if (microtimeObject && typeof (timer.ns = microtimeObject.now) == 'function') {
        timers.push({ 'ns': timer.ns,  'res': getRes('us'), 'unit': 'us' });
      }

     */

    val Default: Clock =
      Chrome getOrElse SysNano

    println("Clock: " + Default)
  }
}
