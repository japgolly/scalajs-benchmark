package demo.suites.scala

import demo.Util._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.GenIso
import scala.collection.immutable._

object SetBuilding {

  case class Params(uniq: Int, dups: Int)

  type A = Int
  val bm = Benchmark.setup[Params, List[A]] { p =>
    ((0 until p.uniq).iterator ++ Array.fill(p.dups)(0)).toList
  }

  val suite = Suite("Set building")(
    bm("Set.fold")(as =>
      as.foldLeft(Set.empty[A])(_ + _)
    ),

    bm("Set.newBuilder") { as =>
      val b = Set.newBuilder[A]
      as foreach (b += _)
      b.result()
    },

    bm("HashSet.fold")(as =>
      as.foldLeft(HashSet.empty[A])(_ + _)
    ),

    bm("HashSet.newBuilder") { as =>
      val b = HashSet.newBuilder[A]
      as foreach (b += _)
      b.result()
    },

    bm("TreeSet.fold")(as =>
      as.foldLeft(TreeSet.empty[A])(_ + _)
    ),

    bm("TreeSet.newBuilder") { as =>
      val b = TreeSet.newBuilder[A]
      as foreach (b += _)
      b.result()
    },

    bm("ListSet.fold")(as =>
      as.foldLeft(ListSet.empty[A])(_ + _)
    ).setDisabledByDefault,

    bm("ListSet.newBuilder") { as =>
      val b = ListSet.newBuilder[A]
      as foreach (b += _)
      b.result()
    }
  )

  val iso = GenIso.fields[Params]

  val param1 = GuiParam.int("Unique elements", 1000)
  val param2 = GuiParam.int("Duplicates", 0, 100)
  val params = GuiParams.combine2(iso)(param1, param2)

  val guiSuite = GuiSuite(suite, params).describe(
    <.div(<.div(^.marginBottom := "0.8em",
      "Experiments to see how significant the differences are in the various ways of building up a ",
      <.code("Set[_]"), "."),
      linkToSource(sourceFilename)))

}
