package japgolly.scalajs.benchmark.vendor

import org.scalajs.dom.raw.Blob
import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.annotation._

@JSGlobal("window")
@js.native
@nowarn("cat=unused")
object FileSaver extends js.Object {

  def saveAs(blob: Blob, filename: String = js.native): Unit = js.native
}
