package demo.suites.scala

import demo.Util._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.vdom.prefix_<^._
import scala.collection.mutable

object MapBuilding {

  type K = String
  type V = String
  type Result = Map[K, V]

  val bm = Benchmark.setup[Int, List[(K, V)]](size =>
    // Puts it in a non-linear, deterministic order then change to disrupt hash order
    (-size to -1).toSet.iterator.map((i: Int) => ((-i).toString, i.toString)).toList)

  // Ensure that the sample data tuple isn't used as is
  // In nearly all cases where I build maps the data doesn't come as tuples and if it does, it's not due to upstream.
  // Avoiding the tuple-boxing is better.
  def iterate(kvs: List[(K, V)])(f: (K, V) => Unit): Unit =
    kvs.foreach(t => f(t._1, t._2))

  val suite = Suite("Map building")(

    bm("Map builder"){ kvs =>
      val b = Map.newBuilder[K, V]
      iterate(kvs)((k, v) => b += ((k, v)))
      b.result(): Result
    },

    bm("Map var + .updated"){ kvs =>
      var m = Map.empty[K, V]
      iterate(kvs)((k, v) => m = m.updated(k, v))
      m: Result
    },

    bm("Mutable Map + .update"){ kvs =>
      val m = mutable.Map.empty[K, V]
      iterate(kvs)((k, v) => m.update(k, v))
      m.toMap: Result
    }.setDisabledByDefault
  )

  val param = GuiParam.int("Size", 10, 100, 1000)

  val guiSuite = GuiSuite(suite, param).describe(
    <.div(
      <.div("This explores different ways of building an immutable ", <.code("Map"), "."),
      <.div("Using a map builder forces tuple-boxing which can be avoided by using ", <.code(".update(d)"), "."),
      linkToSource(sourceFilename))
  )
}
