package japgolly.scalajs.benchmark.gui

final case class UrlFrag(path: String)

object UrlFrag {
  def from(string: String): UrlFrag =
    UrlFrag(string.toLowerCase.replaceAll("[^a-zA-Z0-9-]+", "_"))
}
