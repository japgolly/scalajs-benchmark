package japgolly.scalajs.benchmark.engine

class Blackhole {
  var x = false
  var ar: AnyRef = ""

  final def consume(obj: AnyRef): Unit =
    x ^= obj eq ar

  final def consume(objs: Array[AnyRef]): Unit =
    x ^= objs eq ar

  final def consume(b: Byte): Unit =
    x ^= b == 0

  final def consume(bool: Boolean): Unit =
    x ^= bool

  final def consume(c: Char): Unit =
    x ^= c == 0

  final def consume(s: Short): Unit =
    x ^= s == 0

  final def consume(i: Int): Unit =
    x ^= i == 0

  final def consume(l: Long): Unit =
    x ^= l.toInt == 0

  final def consume(f: Float): Unit =
    x ^= f == 0

  final def consume(d: Double): Unit =
    x ^= d == 0

  final def consumeA[A](a: A): Unit =
    consume(a :: Nil)
}
