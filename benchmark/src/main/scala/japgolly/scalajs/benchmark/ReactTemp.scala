package japgolly.scalajs.benchmark

import japgolly.scalajs.react._
import StateAccess._

object ReactTemp {

  private val tryUnit = scala.util.Success(())

  implicit class SJRModStateExt[S](private val self: ModState[CallbackTo, S]) extends AnyVal {
    def modStateAsync(f: S => S): AsyncCallback[Unit] =
      AsyncCallback(complete => self.modState(f, complete(tryUnit)))
  }


}
