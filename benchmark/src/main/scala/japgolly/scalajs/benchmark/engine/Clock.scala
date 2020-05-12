package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.react.CallbackTo
import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.util.Try

trait Clock {
  def time(f: CallbackTo[_]): CallbackTo[FiniteDuration]
}

trait StatelessClock extends Clock {
  type Time
  def unsafeGet(): Time
  def duration(start: Time, end: Time): FiniteDuration

  // val bh = new Blackhole

  override def time(c: CallbackTo[_]): CallbackTo[FiniteDuration] = {
    val f = c.toScalaFn
    CallbackTo {
      val a = unsafeGet()
      val x = f()
      val b = unsafeGet()
      // bh.consumeA(x)
      duration(a, b)
    }
  }
}

object Clock {
  object SysNano extends StatelessClock {
    override def toString = "SysNano"
    override type Time                      = Long
    override def unsafeGet()                = System.nanoTime()
    override def duration(a: Time, b: Time) = FiniteDuration(b - a, NANOSECONDS)
  }

  object SysMilli extends StatelessClock {
    override def toString = "SysMilli"
    override type Time                      = Long
    override def unsafeGet()                = System.currentTimeMillis()
    override def duration(a: Time, b: Time) = FiniteDuration(b - a, MILLISECONDS)
  }

  @JSGlobal("chrome.Interval")
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
        override def time(c: CallbackTo[_]): CallbackTo[FiniteDuration] = {
          val f = c.toScalaFn
          CallbackTo {
            i.start()
            val x = f()
            i.stop()
            // bh.consumeA(x)
            FiniteDuration(i.microseconds().toLong, MICROSECONDS)
          }
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
}
