import sbt._

object GenBoilerplate {

  def apply(outputDir: File): File = {

    val pkg = "japgolly.scalajs.benchmark.gui"

    val groups =
      (2 to 22).map { n =>
        val `Pn, En`               = (1 to n).map(i => s"P$i, E$i").mkString(", ")
        val `pn: GuiParam[Pn, En]` = (1 to n).map(i => s"p$i: GuiParam[P$i, E$i]").mkString(", ")
        val pn                     = (1 to n).map(i => s"p$i").mkString(", ")
        val `(Pn)`                 = (1 to n).map(i => s"P$i").mkString("(", ", ", ")")
        val spn                    = (1 to n).map(i => s"sp$i").mkString(", ")
        val `an <- vn`             = (1 to n).map(i => s"a$i <- v$i").mkString("; ")
        val an                     = (1 to n).map(i => s"a$i").mkString(", ")

        val subParams =
          (1 to n).map { i =>
            val newTuple = (1 to n).map(j => if (j == i) "n" else s"t._$j").mkString("(", ", ", ")")
            s"    val sp$i = SubParam(${i-1}, p$i, iso andThen Lens[${`(Pn)`}, P$i](_._$i)(n => t => $newTuple))"
          }.mkString("\n")

        val parses =
          (1 to n).map { i =>
            s"          v$i <- sp$i.parse(sp$i.key.get(s)).toRight(sp$i.param.header)"
          }.mkString("\n")

        s"""
           |  final def combine$n[P, ${`Pn, En`}](iso: Iso[P, ${`(Pn)`}])(${`pn: GuiParam[Pn, En]`}): GuiParams[P] = {
           |$subParams
           |    val sps = Vector($spn)
           |    new MostlyGenericParams(sps) {
           |      override def parseState(s: GenState): ParseResult[P] =
           |        for {
           |$parses
           |        } yield
           |          for {${`an <- vn`}} yield iso.reverseGet(($an))
           |    }
           |  }
           |
           |  final def tuple$n[${`Pn, En`}](${`pn: GuiParam[Pn, En]`}): GuiParams[${`(Pn)`}] =
           |    combine$n(Iso.id[${`(Pn)`}])($pn)
         """.stripMargin.trim.replaceFirst("^", "  ")
      }

    // val sep = s"\n  // ${"=" * 115}\n\n"

    val content =
      s"""
         |package $pkg
         |
         |import monocle.{Iso, Lens}
         |
         |abstract class GuiParamsBoilerplate {
         |  self: GuiParams.type =>
         |  import Internals._
         |
         |${groups.mkString("\n\n")}
         |}
        """.stripMargin.trim

    val file = (outputDir / pkg.replace('.', '/') / "boilerplate.scala").asFile
    IO.write(file, content)
    println(s"Generated ${file.getAbsolutePath}")
    file
  }
}
