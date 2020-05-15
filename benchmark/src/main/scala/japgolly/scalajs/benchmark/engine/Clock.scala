package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.react.CallbackTo
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.util.Try

trait Clock {
  /** @return time `f` took to complete, in milliseconds */
  def time(f: CallbackTo[_]): CallbackTo[Double]
}

trait StatelessClock extends Clock {
  type Time
  def unsafeGet(): Time

  /** @return difference in milliseconds */
  def duration(start: Time, end: Time): Double

  // val bh = new Blackhole

  override def time(c: CallbackTo[_]): CallbackTo[Double] = {
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
    override def duration(a: Time, b: Time) = (b - a).toDouble / 1000000
  }

  object SysMilli extends StatelessClock {
    override def toString = "SysMilli"
    override type Time                      = Long
    override def unsafeGet()                = System.currentTimeMillis()
    override def duration(a: Time, b: Time) = (b - a).toDouble
  }

  @JSGlobal("chrome.Interval")
  @js.native
  class ChromeInterval() extends js.Any {
    def start(): Unit = js.native
    def stop(): Unit = js.native
    def microseconds(): Double = js.native
  }

  val Chrome: Option[Clock] =
    Try(new ChromeInterval()).toOption.map { i =>
      var last: Any = 123 // serving the same purpose as BlackHole

      new Clock {
        override def toString =
          last match {
            case notGonnaHappen: Clock => notGonnaHappen.toString
            case _                     => "Clock.Chrome"
          }

        override def time(c: CallbackTo[_]): CallbackTo[Double] = {
          val f = c.toScalaFn
          CallbackTo {
            i.start()
            last = f()
            i.stop()
            i.microseconds() / 1000.0
          }
        }
      }
    }

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
