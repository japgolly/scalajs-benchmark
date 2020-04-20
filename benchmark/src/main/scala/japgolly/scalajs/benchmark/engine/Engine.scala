package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark._
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.timers.SetTimeoutHandle

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

final case class AbortFn(value: AsyncCallback[Unit]) {
  val callback = value.toCallback
}

object Engine {

  private class Ref[A](var value: A)

  /** Runs a suite of benchmarks (asynchronously).
    *
    * You are required to handle events in order to extract any meaning.
    *
    * @return A function by which the benchmarks can be aborted.
    */
  def run[P](plan   : Plan[P],
             options: Options = Options.Default)
            (onEvent: Event[P] => AsyncCallback[Unit]): CallbackTo[AbortFn] = CallbackTo {

    val hnd                    = new Ref[UndefOr[SetTimeoutHandle]](js.undefined)
    var broadcastFinishOnAbort = false
    var progress               = Progress(plan, 0)

    def isEnough(s: Stats.Mutable): Boolean = {
      import options._
      @inline def small = s.runs >= minRuns && s.totalTime >= minTime
      @inline def large = s.runs >= maxRuns || s.totalTime >= maxTime
      small || large
    }

    val finish: AsyncCallback[Unit] =
      AsyncCallback.point {
        hnd.value = js.undefined
        broadcastFinishOnAbort = false
        progress
      }.flatMap(p => onEvent(SuiteFinished(p)))

    val runAsync: AsyncCallback[Unit] = {
      val delay = options.delay()
      val clock = options.clock

      def schedule(next: AsyncCallback[Unit]): AsyncCallback[Unit] =
        AsyncCallback.point {
          hnd.value = js.timers.setTimeout(delay)(next.toCallback.runNow())
        }

      def msg(e: Event[P])(next: AsyncCallback[Unit]): AsyncCallback[Unit] =
        onEvent(e) >> schedule(next)

      def go(keys: List[PlanKey[P]]): AsyncCallback[Unit] = keys match {
        case key :: next =>
          msg(BenchmarkPreparing(progress, key)) {

            def complete(result: Result): AsyncCallback[Unit] =
              AsyncCallback.point { progress = progress.copy(runs = progress.runs + 1) } >>
              msg(BenchmarkFinished(progress, key, result))(
                go(next))

            val setup =
              AsyncCallback.point(key.bm.setup.run(key.param))

              setup.attempt.flatMap {
                case Left(err) =>
                  complete(Left(err))

                case Right((fn, teardown)) =>

                  msg(BenchmarkRunning(progress, key)) {
                    val benchmark: AsyncCallback[Stats] =
                      AsyncCallback.point {
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

                        Stats(rs.times.toVector, options)
                      }

                    benchmark
                      .attempt
                      .finallyRun(teardown.asAsyncCallback)
                      .flatMap(complete)
                  }
                }
              }

        case Nil =>
          finish
      }

      msg(SuiteStarting(progress)) {
        broadcastFinishOnAbort = true
        go(plan.keys)
      }
    }

    // Schedule to start
    hnd.value = js.timers.setTimeout(options.initialDelay)(runAsync.toCallback.runNow())

    AbortFn(
      for {
        _ <- AsyncCallback.point(hnd.value foreach js.timers.clearTimeout)
        b <- AsyncCallback.point(broadcastFinishOnAbort)
        _ <- finish.when_(b)
      } yield ()
    )
  }

  /** Runs a suite of benchmarks (asynchronously), printing details and results to the console.
    *
    * @return A function by which the benchmarks can be aborted.
    */
  def runToConsole[P](plan: Plan[P], options: Options = Options.Default): CallbackTo[AbortFn] = {
    val fmt = {
      val prog  = plan.totalBenchmarks.toString.length
      val name  = plan.bms.foldLeft(0)(_ max _.name.length)
      var param = plan.params.foldLeft(0)(_ max _.toString.length)
      if (!plan.params.forall(_.toString.matches("^-?\\d+$")))
        param = -param
      s"[%${prog}d/%d] %-${name}s %${param}s : %s"
    }

    run(plan, options) {
      case SuiteStarting     (p)       => Callback.log("Starting suite: " + p.plan.name).asAsyncCallback
      case BenchmarkPreparing(p, k)    => AsyncCallback.unit
      case BenchmarkRunning  (p, k)    => AsyncCallback.unit
      case BenchmarkFinished (p, k, r) => Callback.info(fmt.format(p.runs, p.total, k.bm.name, k.param, r)).asAsyncCallback
      case SuiteFinished     (p)       => Callback.log("Suite completed: " + p.plan.name).asAsyncCallback
    }
  }

}
