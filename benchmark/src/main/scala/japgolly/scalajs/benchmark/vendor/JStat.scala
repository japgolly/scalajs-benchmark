package japgolly.scalajs.benchmark.vendor

import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSGlobal("jStat")
@js.native
object JStat extends js.Object {

  @js.native
  object studentt extends js.Object {
    def inv(p: Double, df: Int): Double = js.native
  }

}
