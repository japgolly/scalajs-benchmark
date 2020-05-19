package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.react.Reusability
import japgolly.univeq.UnivEq

sealed trait Validity {
  def unary_! : Validity
  def &&(e: Validity): Validity

  final def is(e: Validity): Boolean =
    this eq e

  final def when(cond: Boolean): Validity =
    if (cond) this else !this

  @inline final def unless(cond: Boolean): Validity =
    when(!cond)
}

case object Valid extends Validity {
  override def unary_! = Invalid
  override def &&(e: Validity) = e
}

case object Invalid extends Validity {
  override def unary_! = Valid
  override def &&(e: Validity) = this
}

object Validity {
  implicit def univEq: UnivEq[Validity] = UnivEq.derive
  implicit val reusability: Reusability[Validity] = Reusability.by_==
}