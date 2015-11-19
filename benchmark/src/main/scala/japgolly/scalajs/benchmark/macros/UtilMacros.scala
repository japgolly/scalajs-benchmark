package japgolly.scalajs.benchmark.macros

import scala.reflect.macros.blackbox.Context

object UtilMacros {
  def sourceFilename: String = macro UtilMacrosImpl.sourceFilename
}


final class UtilMacrosImpl(val c: Context) {
  import c.universe._

  def sourceFilename: c.Expr[String] = {
    val s = c.enclosingPosition.source.file.path.replace('\\', '/')
    c.Expr[String](Literal(Constant(s)))
  }
}