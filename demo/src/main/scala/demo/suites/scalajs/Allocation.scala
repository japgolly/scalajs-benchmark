package demo.suites.scalajs

import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import monocle.macros.GenIso

object Allocation {

  final case class Config(classes2: Int)

  val iso    = GenIso.fields[Config]
  val param1 = GuiParam.int("Arity-2 case classes to allocate", 1000000)
  val params = GuiParams.combine1(iso)(param1)

  // ===================================================================================================================

  final case class Class2(arg1: String, arg2: Config)

  type Prep = () => Array[_]

  val bm = Benchmark.setup[Config, Prep] { cfg =>
    val array = new Array[Class2](cfg.classes2)
    val str = "I'm a string"
    () => {
      var i = array.length
      while (i > 0) {
        i -= 1
        array(i) = Class2(str, cfg)
      }
      array
    }
  }

  val suite = Suite("Allocations")(

    bm("Classes")(prep => prep())

  )

  val guiSuite = GuiSuite(suite, params)
}
