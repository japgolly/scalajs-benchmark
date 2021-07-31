package demo.suites.scala

import demo.Util._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable._

object Builders {

  type A = AnyRef

  def newA: A = new AnyRef

  val bm = Benchmark.setup[Int, List[A]](size =>
    List.fill[A](size)(newA))

  val suite = Suite("Collection Building")(

    bm("Array builder") { is =>
      val b = Array.newBuilder[A]
      for (i <- is) b += i
      b.result()
    }.setDisabledByDefault,

    bm("ArraySeq var (append)") { is =>
      var s = ArraySeq.empty[A]
      for (i <- is) s = s :+ i
      s
    }.setDisabledByDefault,

    bm("ArraySeq var (prepend)") { is =>
      var s = ArraySeq.empty[A]
      for (i <- is) s = i +: s
      s
    }.setDisabledByDefault,

    bm("ArraySeq builder") { is =>
      val b = ArraySeq.newBuilder[A]
      for (i <- is) b += i
      b.result()
    },

    bm("LazyList var (append)") { is =>
      var s = LazyList.empty[A]
      for (i <- is) s = i #:: s
      s
    },

    bm("LazyList var (prepend)") { is =>
      var s = LazyList.empty[A]
      for (i <- is) s = i #:: s
      s
    },

    bm("LazyList builder") { is =>
      val b = LazyList.newBuilder[A]
      for (i <- is) b += i
      b.result()
    }.setDisabledByDefault,

    bm("List var (append)") { is =>
      var s = List.empty[A]
      for (i <- is) s = s :+ i
      s
    }.setDisabledByDefault,

    bm("List var (prepend)") { is =>
      var s = List.empty[A]
      for (i <- is) s = i :: s
      s
    },

    bm("List var, prepend & reverse") { is =>
      var s = List.empty[A]
      for (i <- is) s = i :: s
      s.reverse
    },

    bm("List builder") { is =>
      val b = List.newBuilder[A]
      for (i <- is) b += i
      b.result()
    },

    bm("Seq var (append)") { is =>
      var s = Seq.empty[A]
      for (i <- is) s = s :+ i
      s
    }.setDisabledByDefault,

    bm("Seq var (prepend)") { is =>
      var s = Seq.empty[A]
      for (i <- is) s = i +: s
      s
    }.setDisabledByDefault,

    bm("Seq builder") { is =>
      val b = Seq.newBuilder[A]
      for (i <- is) b += i
      b.result()
    },

    bm("Vector var (append)") { is =>
      var s = Vector.empty[A]
      for (i <- is) s = s :+ i
      s
    },

    bm("Vector var (prepend)") { is =>
      var s = Vector.empty[A]
      for (i <- is) s = i +: s
      s
    },

    bm("Vector builder") { is =>
      val b = Vector.newBuilder[A]
      for (i <- is) b += i
      b.result()
    }
  )

  val param = GuiParam.int("Size", 100, 1000, 10000)

  val guiSuite = GuiSuite(suite, param).describe(
    <.div(
      <.div(
        "This explores different ways of building collections, and should highlight the motivation for ", <.code("Builder"),
        "s in Scala stdlib."),
      linkToSource(sourceFilename))
  )
}