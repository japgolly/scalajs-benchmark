package japgolly.scalajs.benchmark.engine

import japgolly.scalajs.react.{AsyncCallback, CallbackTo}
import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.util.Try

trait Clock {
  /** @return time `f` took to complete, in milliseconds */
  def time(f: CallbackTo[Any]): CallbackTo[Double]

  /** @return time `f` took to complete, in milliseconds */
  def timeAsync(f: AsyncCallback[Any]): AsyncCallback[Double]
}

trait StatelessClock extends Clock {
  type Time
  def unsafeGet(): Time

  /** @return difference in milliseconds */
  def duration(start: Time, end: Time): Double

  private var last: Any = 123 // serving the same purpose as BlackHole

  override def toString =
    last match {
      case notGonnaHappen: Clock => notGonnaHappen.toString
      case _                     => "StatelessClock"
    }

  override def time(c: CallbackTo[Any]): CallbackTo[Double] = {
    val f = c.toScalaFn
    CallbackTo {
      val a = unsafeGet()
      last = f()
      val b = unsafeGet()
      duration(a, b)
    }
  }

  override def timeAsync(f: AsyncCallback[Any]): AsyncCallback[Double] =
    for {
      a <- asyncNow
      _ <- f
      b <- asyncNow
    } yield duration(a, b)

  private[this] val asyncNow =
    AsyncCallback.delay(unsafeGet())
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

      @inline def result() = i.microseconds() / 1000.0

      new Clock {
        override def toString =
          last match {
            case notGonnaHappen: Clock => notGonnaHappen.toString
            case _                     => "Clock.Chrome"
          }

        override def time(c: CallbackTo[Any]): CallbackTo[Double] = {
          val f = c.toScalaFn
          CallbackTo {
            i.start()
            last = f()
            i.stop()
            result()
          }
        }

        override def timeAsync(f: AsyncCallback[Any]): AsyncCallback[Double] =
          asyncStart >> f >> asyncStop

        private[this] val asyncStart =
          AsyncCallback.delay(i.start())

        private[this] val asyncStop =
          AsyncCallback.delay {
            i.stop()
            result()
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
