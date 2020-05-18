package japgolly.scalajs.benchmark.vendor

import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSGlobal("jStat")
@js.native
@nowarn("cat=unused")
object JStat extends js.Object {

  @js.native
  object studentt extends js.Object {
    def inv(p: Double, df: Int): Double = js.native
  }

}
