package japgolly.scalajs.benchmark.gui

import japgolly.microlibs.utils.SafeBool
import japgolly.scalajs.react.Reusability

sealed abstract class Validity extends SafeBool.WithBoolOps[Validity] {
  override final def companion = Validity
}

case object Valid extends Validity
case object Invalid extends Validity

object Validity extends SafeBool.Object[Validity] {
  override def positive = Valid
  override def negative = Invalid

  implicit val reusability: Reusability[Validity] = Reusability.by_==
}
