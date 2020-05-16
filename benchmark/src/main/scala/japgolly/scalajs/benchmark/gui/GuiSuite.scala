package japgolly.scalajs.benchmark.gui

import japgolly.scalajs.benchmark._
import japgolly.scalajs.react.vdom.VdomElement
import monocle.Lens

/** A suite of benchmarks with additional info required to slap a GUI on top and present it to the user.
  *
  * If you don't need a GUI, then a plain [[Suite]] is all you need.
  */
final class GuiSuite[P](val suite : Suite[P],
                        val params: GuiParams[P],
                        val desc  : Option[VdomElement]) {

  @inline def name = suite.name

  def describe(e: VdomElement): GuiSuite[P] =
    new GuiSuite(suite, params, Some(e))

  def withBMs(bms: Vector[Benchmark[P]]): GuiSuite[P] =
    GuiSuite.bms[P].set(bms)(this)
}

object GuiSuite {
  def suite[P]: Lens[GuiSuite[P], Suite[P]] =
    Lens((_: GuiSuite[P]).suite)(s => g => new GuiSuite(s, g.params, g.desc))

  def apply(suite: Suite[Unit]): GuiSuite[Unit] =
    new GuiSuite(suite, GuiParams.none, None)

  def apply[P](suite: Suite[P], params: GuiParams[P]): GuiSuite[P] =
    new GuiSuite(suite, params, None)

  def bms[P]: Lens[GuiSuite[P], Vector[Benchmark[P]]] =
    suite[P] ^|-> Suite.bms
}
