package japgolly.scalajs.benchmark.gui

import monocle.{Iso, Lens}

abstract class GuiParamsBoilerplate {
  self: GuiParams.type =>
  import Internals._

  final def combine2[P, P1, E1, P2, E2](iso: Iso[P, (P1, P2)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2), P1](_._1)(n => t => (n, t._2)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2), P2](_._2)(n => t => (t._1, n)))
    val sps = Vector(sp1, sp2)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
        } yield
          for {a1 <- v1; a2 <- v2} yield iso.reverseGet((a1, a2))
    }
  }

  final def tuple2[P1, E1, P2, E2](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2]): GuiParams[(P1, P2)] =
    combine2(Iso.id[(P1, P2)])(p1, p2)

  final def combine3[P, P1, E1, P2, E2, P3, E3](iso: Iso[P, (P1, P2, P3)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3), P1](_._1)(n => t => (n, t._2, t._3)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3), P2](_._2)(n => t => (t._1, n, t._3)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3), P3](_._3)(n => t => (t._1, t._2, n)))
    val sps = Vector(sp1, sp2, sp3)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3} yield iso.reverseGet((a1, a2, a3))
    }
  }

  final def tuple3[P1, E1, P2, E2, P3, E3](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3]): GuiParams[(P1, P2, P3)] =
    combine3(Iso.id[(P1, P2, P3)])(p1, p2, p3)

  final def combine4[P, P1, E1, P2, E2, P3, E3, P4, E4](iso: Iso[P, (P1, P2, P3, P4)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4), P1](_._1)(n => t => (n, t._2, t._3, t._4)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4), P2](_._2)(n => t => (t._1, n, t._3, t._4)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4), P3](_._3)(n => t => (t._1, t._2, n, t._4)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4), P4](_._4)(n => t => (t._1, t._2, t._3, n)))
    val sps = Vector(sp1, sp2, sp3, sp4)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4} yield iso.reverseGet((a1, a2, a3, a4))
    }
  }

  final def tuple4[P1, E1, P2, E2, P3, E3, P4, E4](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4]): GuiParams[(P1, P2, P3, P4)] =
    combine4(Iso.id[(P1, P2, P3, P4)])(p1, p2, p3, p4)

  final def combine5[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5](iso: Iso[P, (P1, P2, P3, P4, P5)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5} yield iso.reverseGet((a1, a2, a3, a4, a5))
    }
  }

  final def tuple5[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5]): GuiParams[(P1, P2, P3, P4, P5)] =
    combine5(Iso.id[(P1, P2, P3, P4, P5)])(p1, p2, p3, p4, p5)

  final def combine6[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6](iso: Iso[P, (P1, P2, P3, P4, P5, P6)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6} yield iso.reverseGet((a1, a2, a3, a4, a5, a6))
    }
  }

  final def tuple6[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6]): GuiParams[(P1, P2, P3, P4, P5, P6)] =
    combine6(Iso.id[(P1, P2, P3, P4, P5, P6)])(p1, p2, p3, p4, p5, p6)

  final def combine7[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7))
    }
  }

  final def tuple7[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7]): GuiParams[(P1, P2, P3, P4, P5, P6, P7)] =
    combine7(Iso.id[(P1, P2, P3, P4, P5, P6, P7)])(p1, p2, p3, p4, p5, p6, p7)

  final def combine8[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8))
    }
  }

  final def tuple8[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8)] =
    combine8(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8)])(p1, p2, p3, p4, p5, p6, p7, p8)

  final def combine9[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9))
    }
  }

  final def tuple9[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9)] =
    combine9(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9)])(p1, p2, p3, p4, p5, p6, p7, p8, p9)

  final def combine10[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10))
    }
  }

  final def tuple10[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10)] =
    combine10(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)

  final def combine11[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11))
    }
  }

  final def tuple11[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11)] =
    combine11(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)

  final def combine12[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11, t._12)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11, t._12)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11, t._12)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11, t._12)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11, t._12)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11, t._12)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n, t._12)))
    val sp12 = SubParam(11, p12, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12), P12](_._12)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
          v12 <- sp12.parse(sp12.key.get(s)).toRight(sp12.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11; a12 <- v12} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12))
    }
  }

  final def tuple12[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)] =
    combine12(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)

  final def combine13[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11, t._12, t._13)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11, t._12, t._13)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11, t._12, t._13)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11, t._12, t._13)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11, t._12, t._13)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n, t._12, t._13)))
    val sp12 = SubParam(11, p12, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P12](_._12)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, n, t._13)))
    val sp13 = SubParam(12, p13, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13), P13](_._13)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
          v12 <- sp12.parse(sp12.key.get(s)).toRight(sp12.param.header)
          v13 <- sp13.parse(sp13.key.get(s)).toRight(sp13.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11; a12 <- v12; a13 <- v13} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13))
    }
  }

  final def tuple13[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13)] =
    combine13(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)

  final def combine14[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11, t._12, t._13, t._14)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11, t._12, t._13, t._14)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11, t._12, t._13, t._14)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11, t._12, t._13, t._14)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n, t._12, t._13, t._14)))
    val sp12 = SubParam(11, p12, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P12](_._12)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, n, t._13, t._14)))
    val sp13 = SubParam(12, p13, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P13](_._13)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, n, t._14)))
    val sp14 = SubParam(13, p14, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14), P14](_._14)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13, sp14)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
          v12 <- sp12.parse(sp12.key.get(s)).toRight(sp12.param.header)
          v13 <- sp13.parse(sp13.key.get(s)).toRight(sp13.param.header)
          v14 <- sp14.parse(sp14.key.get(s)).toRight(sp14.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11; a12 <- v12; a13 <- v13; a14 <- v14} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14))
    }
  }

  final def tuple14[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14)] =
    combine14(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)

  final def combine15[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11, t._12, t._13, t._14, t._15)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11, t._12, t._13, t._14, t._15)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11, t._12, t._13, t._14, t._15)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n, t._12, t._13, t._14, t._15)))
    val sp12 = SubParam(11, p12, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P12](_._12)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, n, t._13, t._14, t._15)))
    val sp13 = SubParam(12, p13, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P13](_._13)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, n, t._14, t._15)))
    val sp14 = SubParam(13, p14, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P14](_._14)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, n, t._15)))
    val sp15 = SubParam(14, p15, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15), P15](_._15)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13, sp14, sp15)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
          v12 <- sp12.parse(sp12.key.get(s)).toRight(sp12.param.header)
          v13 <- sp13.parse(sp13.key.get(s)).toRight(sp13.param.header)
          v14 <- sp14.parse(sp14.key.get(s)).toRight(sp14.param.header)
          v15 <- sp15.parse(sp15.key.get(s)).toRight(sp15.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11; a12 <- v12; a13 <- v13; a14 <- v14; a15 <- v15} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15))
    }
  }

  final def tuple15[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15)] =
    combine15(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)

  final def combine16[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11, t._12, t._13, t._14, t._15, t._16)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11, t._12, t._13, t._14, t._15, t._16)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n, t._12, t._13, t._14, t._15, t._16)))
    val sp12 = SubParam(11, p12, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P12](_._12)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, n, t._13, t._14, t._15, t._16)))
    val sp13 = SubParam(12, p13, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P13](_._13)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, n, t._14, t._15, t._16)))
    val sp14 = SubParam(13, p14, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P14](_._14)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, n, t._15, t._16)))
    val sp15 = SubParam(14, p15, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P15](_._15)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, n, t._16)))
    val sp16 = SubParam(15, p16, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16), P16](_._16)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13, sp14, sp15, sp16)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
          v12 <- sp12.parse(sp12.key.get(s)).toRight(sp12.param.header)
          v13 <- sp13.parse(sp13.key.get(s)).toRight(sp13.param.header)
          v14 <- sp14.parse(sp14.key.get(s)).toRight(sp14.param.header)
          v15 <- sp15.parse(sp15.key.get(s)).toRight(sp15.param.header)
          v16 <- sp16.parse(sp16.key.get(s)).toRight(sp16.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11; a12 <- v12; a13 <- v13; a14 <- v14; a15 <- v15; a16 <- v16} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16))
    }
  }

  final def tuple16[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16)] =
    combine16(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)

  final def combine17[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11, t._12, t._13, t._14, t._15, t._16, t._17)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n, t._12, t._13, t._14, t._15, t._16, t._17)))
    val sp12 = SubParam(11, p12, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P12](_._12)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, n, t._13, t._14, t._15, t._16, t._17)))
    val sp13 = SubParam(12, p13, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P13](_._13)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, n, t._14, t._15, t._16, t._17)))
    val sp14 = SubParam(13, p14, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P14](_._14)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, n, t._15, t._16, t._17)))
    val sp15 = SubParam(14, p15, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P15](_._15)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, n, t._16, t._17)))
    val sp16 = SubParam(15, p16, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P16](_._16)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, n, t._17)))
    val sp17 = SubParam(16, p17, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17), P17](_._17)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13, sp14, sp15, sp16, sp17)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
          v12 <- sp12.parse(sp12.key.get(s)).toRight(sp12.param.header)
          v13 <- sp13.parse(sp13.key.get(s)).toRight(sp13.param.header)
          v14 <- sp14.parse(sp14.key.get(s)).toRight(sp14.param.header)
          v15 <- sp15.parse(sp15.key.get(s)).toRight(sp15.param.header)
          v16 <- sp16.parse(sp16.key.get(s)).toRight(sp16.param.header)
          v17 <- sp17.parse(sp17.key.get(s)).toRight(sp17.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11; a12 <- v12; a13 <- v13; a14 <- v14; a15 <- v15; a16 <- v16; a17 <- v17} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17))
    }
  }

  final def tuple17[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17)] =
    combine17(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)

  final def combine18[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17, P18, E18](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17], p18: GuiParam[P18, E18]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n, t._12, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp12 = SubParam(11, p12, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P12](_._12)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, n, t._13, t._14, t._15, t._16, t._17, t._18)))
    val sp13 = SubParam(12, p13, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P13](_._13)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, n, t._14, t._15, t._16, t._17, t._18)))
    val sp14 = SubParam(13, p14, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P14](_._14)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, n, t._15, t._16, t._17, t._18)))
    val sp15 = SubParam(14, p15, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P15](_._15)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, n, t._16, t._17, t._18)))
    val sp16 = SubParam(15, p16, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P16](_._16)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, n, t._17, t._18)))
    val sp17 = SubParam(16, p17, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P17](_._17)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, n, t._18)))
    val sp18 = SubParam(17, p18, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18), P18](_._18)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13, sp14, sp15, sp16, sp17, sp18)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
          v12 <- sp12.parse(sp12.key.get(s)).toRight(sp12.param.header)
          v13 <- sp13.parse(sp13.key.get(s)).toRight(sp13.param.header)
          v14 <- sp14.parse(sp14.key.get(s)).toRight(sp14.param.header)
          v15 <- sp15.parse(sp15.key.get(s)).toRight(sp15.param.header)
          v16 <- sp16.parse(sp16.key.get(s)).toRight(sp16.param.header)
          v17 <- sp17.parse(sp17.key.get(s)).toRight(sp17.param.header)
          v18 <- sp18.parse(sp18.key.get(s)).toRight(sp18.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11; a12 <- v12; a13 <- v13; a14 <- v14; a15 <- v15; a16 <- v16; a17 <- v17; a18 <- v18} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18))
    }
  }

  final def tuple18[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17, P18, E18](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17], p18: GuiParam[P18, E18]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18)] =
    combine18(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)

  final def combine19[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17, P18, E18, P19, E19](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17], p18: GuiParam[P18, E18], p19: GuiParam[P19, E19]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp12 = SubParam(11, p12, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P12](_._12)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, n, t._13, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp13 = SubParam(12, p13, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P13](_._13)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, n, t._14, t._15, t._16, t._17, t._18, t._19)))
    val sp14 = SubParam(13, p14, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P14](_._14)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, n, t._15, t._16, t._17, t._18, t._19)))
    val sp15 = SubParam(14, p15, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P15](_._15)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, n, t._16, t._17, t._18, t._19)))
    val sp16 = SubParam(15, p16, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P16](_._16)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, n, t._17, t._18, t._19)))
    val sp17 = SubParam(16, p17, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P17](_._17)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, n, t._18, t._19)))
    val sp18 = SubParam(17, p18, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P18](_._18)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, n, t._19)))
    val sp19 = SubParam(18, p19, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19), P19](_._19)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13, sp14, sp15, sp16, sp17, sp18, sp19)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
          v12 <- sp12.parse(sp12.key.get(s)).toRight(sp12.param.header)
          v13 <- sp13.parse(sp13.key.get(s)).toRight(sp13.param.header)
          v14 <- sp14.parse(sp14.key.get(s)).toRight(sp14.param.header)
          v15 <- sp15.parse(sp15.key.get(s)).toRight(sp15.param.header)
          v16 <- sp16.parse(sp16.key.get(s)).toRight(sp16.param.header)
          v17 <- sp17.parse(sp17.key.get(s)).toRight(sp17.param.header)
          v18 <- sp18.parse(sp18.key.get(s)).toRight(sp18.param.header)
          v19 <- sp19.parse(sp19.key.get(s)).toRight(sp19.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11; a12 <- v12; a13 <- v13; a14 <- v14; a15 <- v15; a16 <- v16; a17 <- v17; a18 <- v18; a19 <- v19} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19))
    }
  }

  final def tuple19[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17, P18, E18, P19, E19](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17], p18: GuiParam[P18, E18], p19: GuiParam[P19, E19]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19)] =
    combine19(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19)

  final def combine20[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17, P18, E18, P19, E19, P20, E20](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17], p18: GuiParam[P18, E18], p19: GuiParam[P19, E19], p20: GuiParam[P20, E20]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp12 = SubParam(11, p12, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P12](_._12)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, n, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp13 = SubParam(12, p13, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P13](_._13)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, n, t._14, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp14 = SubParam(13, p14, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P14](_._14)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, n, t._15, t._16, t._17, t._18, t._19, t._20)))
    val sp15 = SubParam(14, p15, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P15](_._15)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, n, t._16, t._17, t._18, t._19, t._20)))
    val sp16 = SubParam(15, p16, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P16](_._16)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, n, t._17, t._18, t._19, t._20)))
    val sp17 = SubParam(16, p17, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P17](_._17)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, n, t._18, t._19, t._20)))
    val sp18 = SubParam(17, p18, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P18](_._18)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, n, t._19, t._20)))
    val sp19 = SubParam(18, p19, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P19](_._19)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, n, t._20)))
    val sp20 = SubParam(19, p20, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20), P20](_._20)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13, sp14, sp15, sp16, sp17, sp18, sp19, sp20)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
          v12 <- sp12.parse(sp12.key.get(s)).toRight(sp12.param.header)
          v13 <- sp13.parse(sp13.key.get(s)).toRight(sp13.param.header)
          v14 <- sp14.parse(sp14.key.get(s)).toRight(sp14.param.header)
          v15 <- sp15.parse(sp15.key.get(s)).toRight(sp15.param.header)
          v16 <- sp16.parse(sp16.key.get(s)).toRight(sp16.param.header)
          v17 <- sp17.parse(sp17.key.get(s)).toRight(sp17.param.header)
          v18 <- sp18.parse(sp18.key.get(s)).toRight(sp18.param.header)
          v19 <- sp19.parse(sp19.key.get(s)).toRight(sp19.param.header)
          v20 <- sp20.parse(sp20.key.get(s)).toRight(sp20.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11; a12 <- v12; a13 <- v13; a14 <- v14; a15 <- v15; a16 <- v16; a17 <- v17; a18 <- v18; a19 <- v19; a20 <- v20} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20))
    }
  }

  final def tuple20[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17, P18, E18, P19, E19, P20, E20](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17], p18: GuiParam[P18, E18], p19: GuiParam[P19, E19], p20: GuiParam[P20, E20]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20)] =
    combine20(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)

  final def combine21[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17, P18, E18, P19, E19, P20, E20, P21, E21](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17], p18: GuiParam[P18, E18], p19: GuiParam[P19, E19], p20: GuiParam[P20, E20], p21: GuiParam[P21, E21]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp12 = SubParam(11, p12, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P12](_._12)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, n, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp13 = SubParam(12, p13, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P13](_._13)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, n, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp14 = SubParam(13, p14, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P14](_._14)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, n, t._15, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp15 = SubParam(14, p15, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P15](_._15)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, n, t._16, t._17, t._18, t._19, t._20, t._21)))
    val sp16 = SubParam(15, p16, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P16](_._16)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, n, t._17, t._18, t._19, t._20, t._21)))
    val sp17 = SubParam(16, p17, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P17](_._17)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, n, t._18, t._19, t._20, t._21)))
    val sp18 = SubParam(17, p18, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P18](_._18)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, n, t._19, t._20, t._21)))
    val sp19 = SubParam(18, p19, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P19](_._19)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, n, t._20, t._21)))
    val sp20 = SubParam(19, p20, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P20](_._20)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, n, t._21)))
    val sp21 = SubParam(20, p21, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21), P21](_._21)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13, sp14, sp15, sp16, sp17, sp18, sp19, sp20, sp21)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
          v12 <- sp12.parse(sp12.key.get(s)).toRight(sp12.param.header)
          v13 <- sp13.parse(sp13.key.get(s)).toRight(sp13.param.header)
          v14 <- sp14.parse(sp14.key.get(s)).toRight(sp14.param.header)
          v15 <- sp15.parse(sp15.key.get(s)).toRight(sp15.param.header)
          v16 <- sp16.parse(sp16.key.get(s)).toRight(sp16.param.header)
          v17 <- sp17.parse(sp17.key.get(s)).toRight(sp17.param.header)
          v18 <- sp18.parse(sp18.key.get(s)).toRight(sp18.param.header)
          v19 <- sp19.parse(sp19.key.get(s)).toRight(sp19.param.header)
          v20 <- sp20.parse(sp20.key.get(s)).toRight(sp20.param.header)
          v21 <- sp21.parse(sp21.key.get(s)).toRight(sp21.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11; a12 <- v12; a13 <- v13; a14 <- v14; a15 <- v15; a16 <- v16; a17 <- v17; a18 <- v18; a19 <- v19; a20 <- v20; a21 <- v21} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21))
    }
  }

  final def tuple21[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17, P18, E18, P19, E19, P20, E20, P21, E21](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17], p18: GuiParam[P18, E18], p19: GuiParam[P19, E19], p20: GuiParam[P20, E20], p21: GuiParam[P21, E21]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21)] =
    combine21(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21)

  final def combine22[P, P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17, P18, E18, P19, E19, P20, E20, P21, E21, P22, E22](iso: Iso[P, (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22)])(p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17], p18: GuiParam[P18, E18], p19: GuiParam[P19, E19], p20: GuiParam[P20, E20], p21: GuiParam[P21, E21], p22: GuiParam[P22, E22]): GuiParams[P] = {
    val sp1 = SubParam(0, p1, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P1](_._1)(n => t => (n, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp2 = SubParam(1, p2, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P2](_._2)(n => t => (t._1, n, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp3 = SubParam(2, p3, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P3](_._3)(n => t => (t._1, t._2, n, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp4 = SubParam(3, p4, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P4](_._4)(n => t => (t._1, t._2, t._3, n, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp5 = SubParam(4, p5, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P5](_._5)(n => t => (t._1, t._2, t._3, t._4, n, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp6 = SubParam(5, p6, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P6](_._6)(n => t => (t._1, t._2, t._3, t._4, t._5, n, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp7 = SubParam(6, p7, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P7](_._7)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, n, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp8 = SubParam(7, p8, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P8](_._8)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, n, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp9 = SubParam(8, p9, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P9](_._9)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, n, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp10 = SubParam(9, p10, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P10](_._10)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, n, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp11 = SubParam(10, p11, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P11](_._11)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, n, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp12 = SubParam(11, p12, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P12](_._12)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, n, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp13 = SubParam(12, p13, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P13](_._13)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, n, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp14 = SubParam(13, p14, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P14](_._14)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, n, t._15, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp15 = SubParam(14, p15, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P15](_._15)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, n, t._16, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp16 = SubParam(15, p16, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P16](_._16)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, n, t._17, t._18, t._19, t._20, t._21, t._22)))
    val sp17 = SubParam(16, p17, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P17](_._17)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, n, t._18, t._19, t._20, t._21, t._22)))
    val sp18 = SubParam(17, p18, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P18](_._18)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, n, t._19, t._20, t._21, t._22)))
    val sp19 = SubParam(18, p19, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P19](_._19)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, n, t._20, t._21, t._22)))
    val sp20 = SubParam(19, p20, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P20](_._20)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, n, t._21, t._22)))
    val sp21 = SubParam(20, p21, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P21](_._21)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, n, t._22)))
    val sp22 = SubParam(21, p22, iso andThen Lens[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22), P22](_._22)(n => t => (t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16, t._17, t._18, t._19, t._20, t._21, n)))
    val sps = Vector(sp1, sp2, sp3, sp4, sp5, sp6, sp7, sp8, sp9, sp10, sp11, sp12, sp13, sp14, sp15, sp16, sp17, sp18, sp19, sp20, sp21, sp22)
    new MostlyGenericParams(sps) {
      override def parseState(s: GenState): ParseResult[P] =
        for {
          v1 <- sp1.parse(sp1.key.get(s)).toRight(sp1.param.header)
          v2 <- sp2.parse(sp2.key.get(s)).toRight(sp2.param.header)
          v3 <- sp3.parse(sp3.key.get(s)).toRight(sp3.param.header)
          v4 <- sp4.parse(sp4.key.get(s)).toRight(sp4.param.header)
          v5 <- sp5.parse(sp5.key.get(s)).toRight(sp5.param.header)
          v6 <- sp6.parse(sp6.key.get(s)).toRight(sp6.param.header)
          v7 <- sp7.parse(sp7.key.get(s)).toRight(sp7.param.header)
          v8 <- sp8.parse(sp8.key.get(s)).toRight(sp8.param.header)
          v9 <- sp9.parse(sp9.key.get(s)).toRight(sp9.param.header)
          v10 <- sp10.parse(sp10.key.get(s)).toRight(sp10.param.header)
          v11 <- sp11.parse(sp11.key.get(s)).toRight(sp11.param.header)
          v12 <- sp12.parse(sp12.key.get(s)).toRight(sp12.param.header)
          v13 <- sp13.parse(sp13.key.get(s)).toRight(sp13.param.header)
          v14 <- sp14.parse(sp14.key.get(s)).toRight(sp14.param.header)
          v15 <- sp15.parse(sp15.key.get(s)).toRight(sp15.param.header)
          v16 <- sp16.parse(sp16.key.get(s)).toRight(sp16.param.header)
          v17 <- sp17.parse(sp17.key.get(s)).toRight(sp17.param.header)
          v18 <- sp18.parse(sp18.key.get(s)).toRight(sp18.param.header)
          v19 <- sp19.parse(sp19.key.get(s)).toRight(sp19.param.header)
          v20 <- sp20.parse(sp20.key.get(s)).toRight(sp20.param.header)
          v21 <- sp21.parse(sp21.key.get(s)).toRight(sp21.param.header)
          v22 <- sp22.parse(sp22.key.get(s)).toRight(sp22.param.header)
        } yield
          for {a1 <- v1; a2 <- v2; a3 <- v3; a4 <- v4; a5 <- v5; a6 <- v6; a7 <- v7; a8 <- v8; a9 <- v9; a10 <- v10; a11 <- v11; a12 <- v12; a13 <- v13; a14 <- v14; a15 <- v15; a16 <- v16; a17 <- v17; a18 <- v18; a19 <- v19; a20 <- v20; a21 <- v21; a22 <- v22} yield iso.reverseGet((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22))
    }
  }

  final def tuple22[P1, E1, P2, E2, P3, E3, P4, E4, P5, E5, P6, E6, P7, E7, P8, E8, P9, E9, P10, E10, P11, E11, P12, E12, P13, E13, P14, E14, P15, E15, P16, E16, P17, E17, P18, E18, P19, E19, P20, E20, P21, E21, P22, E22](p1: GuiParam[P1, E1], p2: GuiParam[P2, E2], p3: GuiParam[P3, E3], p4: GuiParam[P4, E4], p5: GuiParam[P5, E5], p6: GuiParam[P6, E6], p7: GuiParam[P7, E7], p8: GuiParam[P8, E8], p9: GuiParam[P9, E9], p10: GuiParam[P10, E10], p11: GuiParam[P11, E11], p12: GuiParam[P12, E12], p13: GuiParam[P13, E13], p14: GuiParam[P14, E14], p15: GuiParam[P15, E15], p16: GuiParam[P16, E16], p17: GuiParam[P17, E17], p18: GuiParam[P18, E18], p19: GuiParam[P19, E19], p20: GuiParam[P20, E20], p21: GuiParam[P21, E21], p22: GuiParam[P22, E22]): GuiParams[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22)] =
    combine22(Iso.id[(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22)])(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21, p22)
}