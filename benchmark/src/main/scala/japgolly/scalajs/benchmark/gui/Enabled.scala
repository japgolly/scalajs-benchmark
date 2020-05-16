package japgolly.scalajs.benchmark.gui

import japgolly.univeq.UnivEq

sealed trait Enabled {
  def unary_! : Enabled

  final def is(e: Enabled): Boolean =
    this eq e

  final def when(cond: Boolean): Enabled =
    if (cond) this else !this

  @inline final def unless(cond: Boolean): Enabled =
    when(!cond)
}

case object Enabled extends Enabled {
  override def unary_! = Disabled

  implicit def univEq: UnivEq[Enabled] = UnivEq.derive
}

case object Disabled extends Enabled {
  override def unary_! = Enabled
}