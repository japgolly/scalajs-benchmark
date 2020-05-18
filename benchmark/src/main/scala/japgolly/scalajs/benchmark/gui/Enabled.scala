package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react.Reusability
import japgolly.univeq.UnivEq

sealed trait Enabled {
  def unary_! : Enabled
  def &&(e: Enabled): Enabled

  final def is(e: Enabled): Boolean =
    this eq e

  final def when(cond: Boolean): Enabled =
    if (cond) this else !this

  @inline final def unless(cond: Boolean): Enabled =
    when(!cond)
}

case object Enabled extends Enabled {
  override def unary_! = Disabled
  override def &&(e: Enabled) = e

  implicit def univEq: UnivEq[Enabled] = UnivEq.derive
  implicit val reusability: Reusability[Enabled] = Reusability.by_==
}

case object Disabled extends Enabled {
  override def unary_! = Enabled
  override def &&(e: Enabled) = this
}