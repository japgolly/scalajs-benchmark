package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark._
import japgolly.scalajs.react.Callback
import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.timers.SetTimeoutHandle
import scalaz.{\/, -\/, \/-}

sealed abstract class Event[P] {
  val progress: Progress[P]
  @inline def plan = progress.plan
}

final case class SuiteStarting     [P](progress: Progress[P])                                  extends Event[P]
final case class BenchmarkPreparing[P](progress: Progress[P], key: PlanKey[P])                 extends Event[P]
final case class BenchmarkRunning  [P](progress: Progress[P], key: PlanKey[P])                 extends Event[P]
final case class BenchmarkFinished [P](progress: Progress[P], key: PlanKey[P], result: Result) extends Event[P]
final case class SuiteFinished     [P](progress: Progress[P]/*, aborted | results, */)         extends Event[P]

final case class Progress[P](plan: Plan[P], runs: Int) {
  def total = plan.totalBenchmarks
  def remaining = total - runs
}

final case class AbortFn(run: () => Unit) extends AnyVal {
  def callback = Callback(run())
}

object Engine {

  private class Ref[A](var value: A)

  /**
    * Runs a suite of benchmarks (asynchronously).
    *
    * You are required to handle events in order to extract any meaning.
    *
    * @return A function by which the benchmarks can be aborted.
    */
  def run[P](plan: Plan[P], options: Options = Options.Default)(eventCallback: Event[P] => Callback): AbortFn = {
    val hnd = new Ref[UndefOr[SetTimeoutHandle]](js.undefined)

    def isEnough(s: Stats.Mutable): Boolean = {
      import options._
      @inline def small = s.runs >= minRuns && s.totalTime >= minTime
      @inline def large = s.runs >= maxRuns || s.totalTime >= maxTime
      small || large
    }

    var broadcastFinishOnAbort = false
    var progress = Progress(plan, 0)

    def finish(): Unit = {
      hnd.value = js.undefined
      broadcastFinishOnAbort = false
      eventCallback(SuiteFinished(progress)).runNow()
    }

    def runAsync: Unit = {
      val delay = options.delay()
      val clock = options.clock

      def msg(e: Event[P])(next: => Any): Unit = {
        eventCallback(e).runNow()
        hnd.value = js.timers.setTimeout(delay)(next)
      }

      def go(keys: List[PlanKey[P]]): Unit = keys match {
        case key :: next =>
          msg(BenchmarkPreparing(progress, key)) {

            def complete(result: Result): Unit = {
              progress = progress.copy(runs = progress.runs + 1)
              msg(BenchmarkFinished(progress, key, result))(
                go(next))
            }

            val setupResult =
              \/.fromTryCatchNonFatal {
                key.bm.setup.run(key.param)
              }

              setupResult match {
                case -\/(err) =>
                  complete(-\/(err))

                case \/-((fn, teardown)) =>

                  msg(BenchmarkRunning(progress, key)) {
                    val result: Result =
                      \/.fromTryCatchNonFatal {
                        val rs = new Stats.Mutable

                        @tailrec
                        def go(): Unit = {
                          // val localFnAndTeardown = local.run(())
                          // val t = clock.time(localFnAndTeardown._1())
                          // localFnAndTeardown._2.run()
                          val t = clock.time(fn())
                          rs add t
                          if (!isEnough(rs))
                            go()
                        }
                        go()

                        teardown.run()
                        Stats(rs.times.toVector, options)
                      }

                    complete(result)
                  }
                }
              }

        case Nil =>
          finish()
      }

      msg(SuiteStarting(progress)) {
        broadcastFinishOnAbort = true
        go(plan.keys)
      }
    }

    // Schedule to start
    hnd.value = js.timers.setTimeout(options.initialDelay)(runAsync)

    AbortFn(() => {
      hnd.value foreach js.timers.clearTimeout
      if (broadcastFinishOnAbort)
        finish()
    })
  }

  /**
    * Runs a suite of benchmarks (asynchronously), printing details and results to the console.
    *
    * @return A function by which the benchmarks can be aborted.
    */
  def runToConsole[P](plan: Plan[P], options: Options = Options.Default): AbortFn = {
    val fmt = {
      val prog  = plan.totalBenchmarks.toString.length
      val name  = plan.bms.foldLeft(0)(_ max _.name.length)
      var param = plan.params.foldLeft(0)(_ max _.toString.length)
      if (!plan.params.forall(_.toString.matches("^-?\\d+$")))
        param = -param
      s"[%${prog}d/%d] %-${name}s %${param}s : %s"
    }

    run(plan, options) {
      case SuiteStarting     (p)       => Callback.log("Starting suite: " + p.plan.name)
      case BenchmarkPreparing(p, k)    => Callback.empty
      case BenchmarkRunning  (p, k)    => Callback.empty
      case BenchmarkFinished (p, k, r) => Callback.info(fmt.format(p.runs, p.total, k.bm.name, k.param, r))
      case SuiteFinished     (p)       => Callback.log("Suite completed: " + p.plan.name)
    }
  }

}
