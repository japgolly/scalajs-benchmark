package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.benchmark._
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
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

final case class Progress[P](startedAt    : js.Date,
                             plan         : Plan[P],
                             runs         : Int,
                             engineOptions: EngineOptions) {

  def timestampTxt = TimeUtil.dateStrFromJsDate(startedAt) + "_" + TimeUtil.timeStrFromJsDate(startedAt)
  def total        = plan.totalBenchmarks
  def remaining    = total - runs
}

object Progress {
  def start[P](plan: Plan[P], engineOptions: EngineOptions): Progress[P] =
    apply(new js.Date(), plan, 0, engineOptions)
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
             options: EngineOptions = EngineOptions.default)
            (onEvent: Event[P] => AsyncCallback[Unit]): CallbackTo[AbortFn] = CallbackTo {

    val hnd                    = new Ref[UndefOr[SetTimeoutHandle]](js.undefined)
    var broadcastFinishOnAbort = true
    var aborted                = false
    var progress               = Progress.start(plan, options)
    val setupCtx               = SetupCtx(CallbackTo(aborted))

    val finish: AsyncCallback[Unit] =
      AsyncCallback.delay {
        hnd.value = js.undefined
        broadcastFinishOnAbort = false
        progress
      }.flatMap(p => onEvent(SuiteFinished(p)))

    val runAsync: AsyncCallback[Unit] = {
      val delay = options.delay()
      val clock = options.clock

      def schedule(next: AsyncCallback[Unit]): AsyncCallback[Unit] =
        AsyncCallback.delay {
          hnd.value = js.timers.setTimeout(delay)(next.toCallback.runNow())
        }

      def msg(e: Event[P])(next: AsyncCallback[Unit]): AsyncCallback[Unit] =
        onEvent(e) >> schedule(next)

      def go(keys: List[PlanKey[P]]): AsyncCallback[Unit] = keys match {
        case key :: next =>
          msg(BenchmarkPreparing(progress, key)) {

            def complete(result: Result): AsyncCallback[Unit] =
              AsyncCallback.byName {
                progress = progress.copy(runs = progress.runs + 1)
                msg(BenchmarkFinished(progress, key, result))(go(next))
              }

            val setup =
              key.bm.setup.run(key.param, setupCtx)

            setup.attempt.flatMap {

              case Right((bmFn, teardown)) =>

                // =====================================================================================================
                // Real benchmarking here

                msg(BenchmarkRunning(progress, key)) {
                  val bm = CallbackTo.lift(bmFn)
                  val bmTimedUnsafe = clock.time(bm).toScalaFn

                  def runIteration(sm: Stats.Mutable, maxTimeMs: Double): AsyncCallback[Unit] = {

                    def isEnough(): Boolean =
                      sm.totalBatchTime() >= maxTimeMs

                    val bmRound: AsyncCallback[Unit] =
                      AsyncCallback.delay {
                        val startTime = System.currentTimeMillis()
                        val delayAfter = startTime + 1000
                        @inline def needDelay(): Boolean = System.currentTimeMillis() > delayAfter

                        @tailrec
                        def go(): Unit = {
                          // val localFnAndTeardown = local.run(())
                          // val t = clock.time(localFnAndTeardown._1())
                          // localFnAndTeardown._2.run()
                          val t = bmTimedUnsafe()
                          sm.add(t)
                          if (!isEnough() && !needDelay())
                            go()
                        }

                        if (!aborted)
                          go()
                      }

                    var self: AsyncCallback[Unit] = AsyncCallback.delay(???)
                    self = bmRound >> AsyncCallback.byName {
                      if (!isEnough() && !aborted)
                        self.delayMs(1)
                      else
                        AsyncCallback.delay(sm.endBatch())
                    }
                    self
                  }

                  def runIterations(iterations: Int, maxTime: FiniteDuration): AsyncCallback[() => Stats] =
                    AsyncCallback.byName {
                      val sm        = new Stats.Mutable
                      val iteration = runIteration(sm, TimeUtil.toMs(maxTime))
                      val runs      = (1 to iterations).foldLeft(AsyncCallback.unit)((q, _) => q >> iteration)
                      runs.ret(() => sm.result())
                    }

                  val warmup =
                    runIterations(options.warmupIterations, options.actualWarmupIterationTime)

                  val real =
                    runIterations(options.iterations, options.iterationTime).map(_())

                  (warmup >> real)
                    .attempt
                    .finallyRun(teardown.asAsyncCallback)
                    .flatMap(complete)
                }
                // =====================================================================================================

              case Left(err) =>
                complete(Left(err))
            }
          }

        case Nil =>
          finish
      }

      msg(SuiteStarting(progress)) {
        go(plan.keys)
      }
    }

    // Schedule to start
    hnd.value = js.timers.setTimeout(options.initialDelay)(runAsync.toCallback.runNow())

    AbortFn(
      for {
        _ <- AsyncCallback.delay { aborted = true }
        _ <- AsyncCallback.delay(hnd.value foreach js.timers.clearTimeout)
        b <- AsyncCallback.delay(broadcastFinishOnAbort)
        _ <- finish.when_(b)
      } yield ()
    )
  }

  /** Runs a suite of benchmarks (asynchronously), printing details and results to the console.
    *
    * @return A function by which the benchmarks can be aborted.
    */
  def runToConsole[P](plan: Plan[P], options: EngineOptions = EngineOptions.default): CallbackTo[AbortFn] = {
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
      case BenchmarkPreparing(_, _)    => AsyncCallback.unit
      case BenchmarkRunning  (_, _)    => AsyncCallback.unit
      case BenchmarkFinished (p, k, r) => Callback.info(fmt.format(p.runs, p.total, k.bm.name, k.param, r)).asAsyncCallback
      case SuiteFinished     (p)       => Callback.log("Suite completed: " + p.plan.name).asAsyncCallback
    }
  }

}
