package whatever

//import java.util.concurrent.TimeUnit._
import japgolly.scalajs.react.Callback

import org.scalajs.dom
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.JSName
import scala.scalajs.js.timers.SetTimeoutHandle
import scala.util.Try

object Benchy {

  /*
  case class Suite[I](name: String,
                      bms: Vector[Benchmark[I]],
                      params: Vector[I]) {
  */

  trait Suite {
    type Param

    val name: String
    val bms: Vector[Benchmark[Param]]
    val params: Vector[Param]

    def totalBenchmarks: Int =
      bms.length * params.length

    lazy val keys: List[BMKey] = {
      val bis = bms.indices
      val pis = params.indices
      for {
        pi <- pis.toList
        bi <- bis
      } yield BMKey(bi, pi)
    }

    def withParams(p2: Vector[Param]): Suite.WithParam[Param] =
      Suite[Param](name, bms, p2)
  }

  object Suite {
    type WithParam[A] = Suite {type Param = A}

    def apply[A](_name: String,
                 _bms: Vector[Benchmark[A]],
                 _params: Vector[A]): WithParam[A] =
      new Suite {
        override type Param = A
        override val name   = _name
        override val bms    = _bms
        override val params = _params
      }
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

  case class BMKey(bmIndex: Int, paramIndex: Int) {
    def bm(implicit s: Suite): Benchmark[s.Param] = s.bms(bmIndex)
    def param(implicit s: Suite): s.Param = s.params(paramIndex)
  }

  sealed trait Event
  case class SuiteStarting(p: Progress) extends Event
  case class BenchmarkStarting(p: Progress, key: BMKey) extends Event
  case class BenchmarkFinished(p: Progress, key: BMKey, result: RunResult) extends Event
  case class SuiteFinished(p: Progress /*, aborted | results, */) extends Event

  case class AbortFn(run: () => Unit)

  def runToConsole(s: Suite, delay: FiniteDuration = DefaultDelay): Unit = {
    val fmt = {
      val prog  = s.totalBenchmarks.toString.length
      val name  = s.bms.foldLeft(0)(_ max _.name.length)
      var param = s.params.foldLeft(0)(_ max _.toString.length)
      if (!s.params.forall(_.toString.matches("^-?\\d+$")))
        param = -param
      s"[%${prog}d/%d] %-${name}s %${param}s : %s"
    }

    runSuiteAsync(s, delay) {
      case SuiteStarting    (p)       => Callback.log("Starting...")
      case BenchmarkStarting(p, k)    => Callback.empty
      case BenchmarkFinished(p, k, r) => Callback.log(fmt.format(p.run, p.total, k bm p.s, k param p.s, r))
      case SuiteFinished    (p)       => Callback.log("Done.")
    }
  }

  private class Ref[A](var value: A)

  var minBmDelay = 10.millis

  def runSuiteAsync(s: Suite, delay: FiniteDuration = DefaultDelay)(eh: Event => Callback): AbortFn = {
    val clock = Clock.Default

    val hnd = new Ref[UndefOr[SetTimeoutHandle]](js.undefined)

    def run: Unit = {
      var progress = Progress(s, 0)

      val delay2 = minBmDelay

      def doeh(e: Event)(next: => Any): Unit = {
        eh(e).runNow()
        hnd.value = js.timers.setTimeout(delay2)(next)
      }

      def go(keys: List[BMKey]): Unit = keys match {
        case key :: next =>
          val b = key bm s
          val p = key param s

          doeh(BenchmarkStarting(progress, key)) {

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

            val rr = runBM()

            progress = progress.copy(run = progress.run + 1)
            doeh(BenchmarkFinished(progress, key, rr)) {
              go(next)
            }
          }

        case Nil =>
          eh(SuiteFinished(progress)).runNow()
      }

      doeh(SuiteStarting(progress))(go(s.keys))

    }

    hnd.value = js.timers.setTimeout(delay)(run)
    AbortFn(() => hnd.value foreach js.timers.clearTimeout)
  }

  val DefaultDelay: FiniteDuration = 4.millis

  // suites, benchmarks, and run fns should all take options with shit like the enoughFn, clock

  // ====================================================================================================


  case class Progress(s: Suite, run: Int) {
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
  val minTime = 1.second
  val maxTime = 10.second

//  val minRuns = 10000
//  val minTime = 3.second
//  val maxTime = 30.second

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