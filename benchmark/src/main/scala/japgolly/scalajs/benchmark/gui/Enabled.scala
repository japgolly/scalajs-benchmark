package japgolly.scalajs.benchmark.gui

import japgolly.microlibs.utils.SafeBool
import japgolly.scalajs.react.Reusability

sealed trait Enabled extends SafeBool.WithBoolOps[Enabled] {
  override final def companion = Enabled
}

case object Enabled extends Enabled with SafeBool.Object[Enabled] {
  override def positive = Enabled
  override def negative = Disabled

  implicit val reusability: Reusability[Enabled] = Reusability.by_==
}

case object Disabled extends Enabled
