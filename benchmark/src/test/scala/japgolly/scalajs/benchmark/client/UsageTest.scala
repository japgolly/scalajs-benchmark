package japgolly.scalajs.benchmark.client

/**
  * This tests usage from a library-consumer's point-of-view.
  *
  * Imports are used as advertised.
  * If the imports, implicits, and expectations all pass compilation, then test is a success.
  */
class UsageTest {

  object BMs {
    import japgolly.scalajs.benchmark._, gui._

    def suite = GuiSuite(Suite("Nope")())

    def param = GuiParam(Render.int, Editor.text, Parser.intsAsText)("Size", 5, 10)

    def folder1 = MenuComp.folder("Folder")(suite)
    def folder2 = MenuComp.folder("Folder")(suite, suite)
    def folder3 = MenuComp.folder("Folder")(folder1, folder2)
    def folder4 = MenuComp.folder("Folder")(folder1, suite)
    def folder5 = MenuComp.folder("Folder")(suite, folder1)
    def folder6 = MenuComp.folder("Folder")(suite, suite)
  }

  def main(): Unit = {
    import org.scalajs.dom.document
    import japgolly.scalajs.benchmark.gui.BenchmarkGUI

    def body = document getElementById "body"

    BenchmarkGUI.renderSuite(body)(BMs.suite)


    BenchmarkGUI.renderMenu(body)(BMs.suite)
    BenchmarkGUI.renderMenu(body)(BMs.folder1)
    BenchmarkGUI.renderMenu(body)(BMs.folder1, BMs.folder2)
    BenchmarkGUI.renderMenu(body)(BMs.folder1, BMs.suite)
    BenchmarkGUI.renderMenu(body)(BMs.suite, BMs.folder1)
    BenchmarkGUI.renderMenu(body)(BMs.suite, BMs.suite)
  }
}