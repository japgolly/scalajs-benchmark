package demo.suites.shootouts

import cats.Functor
import demo.Libraries
import demo.Util._
import japgolly.scalajs.benchmark._
import japgolly.scalajs.benchmark.gui._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.GenIso
import scala.util.Random
import scalaz.std.anyVal._
import scalaz.{IMap, Maybe}

object LensShooutout {

  implicit val catsFunctorForMaybe: Functor[Maybe] =
    new Functor[Maybe] {
      override def map[A, B](fa: Maybe[A])(f: A => B) = fa map f
    }

  object BenchModel {
    def safeDivide(a: Int, b: Int): Maybe[Int] = if(b == 0) Maybe.empty else Maybe.just(a / b)

    case class Nested0(s: String, i: Int, n: Nested1, l: Long)
    case class Nested1(s: String, i: Int, n: Nested2, l: Long)
    case class Nested2(s: String, i: Int, n: Nested3, l: Long)
    case class Nested3(s: String, i: Int, n: Nested4, l: Long)
    case class Nested4(s: String, i: Int, n: Nested5, l: Long)
    case class Nested5(s: String, i: Int, n: Nested6, l: Long)
    case class Nested6(s: String, i: Int)

    val r = new Random

    def genInt(): Int = r.nextInt()
    def genLong(): Long = r.nextLong()
    def genStr(): String = r.nextString(r.nextInt(100))

    sealed trait ADT
    case class I(i: Int)    extends ADT
    case class R(r: ADT)    extends ADT

    def getIOption(adt: ADT): Option[Int]    = adt match { case I(i) => Some(i); case _ => None }
    def getROption(adt: ADT): Option[ADT]    = adt match { case R(r) => Some(r); case _ => None }

    def mkI(i: Int)   : ADT = I(i)
    def mkR(r: ADT)   : ADT = R(r)

    case class Point3(x: Int, y: Int, z: Int)
    val p = Point3(2, 10, 24)

    val iMap = IMap.fromList((1 to 200).iterator.map(_ -> 5).toList)

    case class IntWrapper0(i: Int)
    case class IntWrapper1(i: Int)
    case class IntWrapper2(i: Int)
    case class IntWrapper3(i: Int)
    case class IntWrapper4(i: Int)
    case class IntWrapper5(i: Int)
    case class IntWrapper6(i: Int)

    val i = genInt()
    val w0 = IntWrapper0(i)
    val w3 = IntWrapper0(i)
    val w6 = IntWrapper6(i)
  }
  import BenchModel._

  object Input {
    abstract class InputHelper {
      val r = new Random

      def genBool(): Boolean = r.nextBoolean()
      def genInt(): Int = r.nextInt()
      def genLong(): Long = r.nextLong()
      def genStr(): String = r.nextString(r.nextInt(100))
    }

//    class ADTInput extends InputHelper {
//      private def genADT(): ADT =
//        if(genBool()) I(genInt())
//        else R(genADT())
//
//      val adt: ADT = genADT()
//    }

    class Nested0Input extends InputHelper {
      val n0: Nested0 =
        Nested0(genStr(), genInt(),
          Nested1(genStr(), genInt(),
            Nested2(genStr(), genInt(),
              Nested3(genStr(), genInt(),
                Nested4(genStr(), genInt(),
                  Nested5(genStr(), genInt(),
                    Nested6(genStr(), genInt())
                    ,genLong()), genLong()), genLong()), genLong()), genLong()), genLong())
    }
  }
  import Input._

  trait LensBench {
    def lensGet0(in: Nested0Input): Int
    def lensGet3(in: Nested0Input): Int
    def lensGet6(in: Nested0Input): Int

    def lensSet0(in: Nested0Input): Nested0
    def lensSet3(in: Nested0Input): Nested0
    def lensSet6(in: Nested0Input): Nested0

    def lensModify0(in: Nested0Input): Nested0
    def lensModify3(in: Nested0Input): Nested0
    def lensModify6(in: Nested0Input): Nested0

    def lensModifyF0(in: Nested0Input): Maybe[Nested0]
    def lensModifyF3(in: Nested0Input): Maybe[Nested0]
    def lensModifyF6(in: Nested0Input): Maybe[Nested0]
  }

  // ===================================================================================================================
  object MonocleLensBench extends LensBench {
    import monocle.Lens

    val _n1 = Lens[Nested0, Nested1](_.n)(n2 => n1 => n1.copy(n = n2))
    val _n2 = Lens[Nested1, Nested2](_.n)(n3 => n2 => n2.copy(n = n3))
    val _n3 = Lens[Nested2, Nested3](_.n)(n4 => n3 => n3.copy(n = n4))
    val _n4 = Lens[Nested3, Nested4](_.n)(n5 => n4 => n4.copy(n = n5))
    val _n5 = Lens[Nested4, Nested5](_.n)(n6 => n5 => n5.copy(n = n6))
    val _n6 = Lens[Nested5, Nested6](_.n)(n7 => n6 => n6.copy(n = n7))

    val _n0_i = Lens[Nested0, Int](_.i)(i => n => n.copy(i = i))
    val _n3_i = Lens[Nested3, Int](_.i)(i => n => n.copy(i = i))
    val _n6_i = Lens[Nested6, Int](_.i)(i => n => n.copy(i = i))

    val _n0Ton3I = _n1 andThen _n2 andThen _n3 andThen _n3_i
    val _n0Ton6I = _n1 andThen _n2 andThen _n3 andThen _n4 andThen _n5 andThen _n6 andThen _n6_i

    override def lensGet0(in: Nested0Input) = _n0_i.get(in.n0)
    override def lensGet3(in: Nested0Input) = _n0Ton3I.get(in.n0)
    override def lensGet6(in: Nested0Input) = _n0Ton6I.get(in.n0)

    override def lensSet0(in: Nested0Input) = _n0_i.replace(43)(in.n0)
    override def lensSet3(in: Nested0Input) = _n0Ton3I.replace(43)(in.n0)
    override def lensSet6(in: Nested0Input) = _n0Ton6I.replace(43)(in.n0)

    override def lensModify0(in: Nested0Input) = _n0_i.modify(_ + 1)(in.n0)
    override def lensModify3(in: Nested0Input) = _n0Ton3I.modify(_ + 1)(in.n0)
    override def lensModify6(in: Nested0Input) = _n0Ton6I.modify(_ + 1)(in.n0)

    override def lensModifyF0(in: Nested0Input) = _n0_i.modifyF(safeDivide(_, 2))(in.n0)
    override def lensModifyF3(in: Nested0Input) = _n0Ton3I.modifyF(safeDivide(_, 2))(in.n0)
    override def lensModifyF6(in: Nested0Input) = _n0Ton6I.modifyF(safeDivide(_, 2))(in.n0)
  }

  object MonocleMacroLensBench extends LensBench {
    import monocle.macros.GenLens

    val _n1 = GenLens[Nested0](_.n)
    val _n2 = GenLens[Nested1](_.n)
    val _n3 = GenLens[Nested2](_.n)
    val _n4 = GenLens[Nested3](_.n)
    val _n5 = GenLens[Nested4](_.n)
    val _n6 = GenLens[Nested5](_.n)

    val _n0_i = GenLens[Nested0](_.i)
    val _n3_i = GenLens[Nested3](_.i)
    val _n6_i = GenLens[Nested6](_.i)

    val _n0Ton3I = _n1 andThen _n2 andThen _n3 andThen _n3_i
    val _n0Ton6I = _n1 andThen _n2 andThen _n3 andThen _n4 andThen _n5 andThen _n6 andThen _n6_i

    override def lensGet0(in: Nested0Input) = _n0_i.get(in.n0)
    override def lensGet3(in: Nested0Input) = _n0Ton3I.get(in.n0)
    override def lensGet6(in: Nested0Input) = _n0Ton6I.get(in.n0)

    override def lensSet0(in: Nested0Input) = _n0_i.replace(43)(in.n0)
    override def lensSet3(in: Nested0Input) = _n0Ton3I.replace(43)(in.n0)
    override def lensSet6(in: Nested0Input) = _n0Ton6I.replace(43)(in.n0)

    override def lensModify0(in: Nested0Input) = _n0_i.modify(_ + 1)(in.n0)
    override def lensModify3(in: Nested0Input) = _n0Ton3I.modify(_ + 1)(in.n0)
    override def lensModify6(in: Nested0Input) = _n0Ton6I.modify(_ + 1)(in.n0)

    override def lensModifyF0(in: Nested0Input) = _n0_i.modifyF(safeDivide(_, 2))(in.n0)
    override def lensModifyF3(in: Nested0Input) = _n0Ton3I.modifyF(safeDivide(_, 2))(in.n0)
    override def lensModifyF6(in: Nested0Input) = _n0Ton6I.modifyF(safeDivide(_, 2))(in.n0)
  }

  object ScalazLensBench extends LensBench {
    import scalaz.Lens

    val _n1 = Lens.lensg[Nested0, Nested1](n0 => n1 => n0.copy(n = n1), _.n)
    val _n2 = Lens.lensg[Nested1, Nested2](n1 => n2 => n1.copy(n = n2), _.n)
    val _n3 = Lens.lensg[Nested2, Nested3](n2 => n3 => n2.copy(n = n3), _.n)
    val _n4 = Lens.lensg[Nested3, Nested4](n3 => n4 => n3.copy(n = n4), _.n)
    val _n5 = Lens.lensg[Nested4, Nested5](n4 => n5 => n4.copy(n = n5), _.n)
    val _n6 = Lens.lensg[Nested5, Nested6](n5 => n6 => n5.copy(n = n6), _.n)

    val _n0_i = Lens.lensg[Nested0, Int](n => i => n.copy(i = i), _.i)
    val _n3_i = Lens.lensg[Nested3, Int](n => i => n.copy(i = i), _.i)
    val _n6_i = Lens.lensg[Nested6, Int](n => i => n.copy(i = i), _.i)

    val _n0Ton3I = _n1 >=> _n2 >=> _n3 >=> _n3_i
    val _n0Ton6I = _n1 >=> _n2 >=> _n3 >=> _n4 >=> _n5 >=> _n6 >=> _n6_i

    override def lensGet0(in: Nested0Input) = _n0_i.get(in.n0)
    override def lensGet3(in: Nested0Input) = _n0Ton3I.get(in.n0)
    override def lensGet6(in: Nested0Input) = _n0Ton6I.get(in.n0)

    override def lensSet0(in: Nested0Input) = _n0_i.set(in.n0, 43)
    override def lensSet3(in: Nested0Input) = _n0Ton3I.set(in.n0, 43)
    override def lensSet6(in: Nested0Input) = _n0Ton6I.set(in.n0, 43)

    override def lensModify0(in: Nested0Input) = _n0_i.mod(_ + 1, in.n0)
    override def lensModify3(in: Nested0Input) = _n0Ton3I.mod(_ + 1, in.n0)
    override def lensModify6(in: Nested0Input) = _n0Ton6I.mod(_ + 1, in.n0)

    override def lensModifyF0(in: Nested0Input) = _n0_i.modf(safeDivide(_, 2), in.n0)
    override def lensModifyF3(in: Nested0Input) = _n0Ton3I.modf(safeDivide(_, 2), in.n0)
    override def lensModifyF6(in: Nested0Input) = _n0Ton6I.modf(safeDivide(_, 2), in.n0)
  }

  object ShapelessLensBench extends LensBench {
    import shapeless._

    val _n1 = lens[Nested0].n
    val _n2 = lens[Nested1].n
    val _n3 = lens[Nested2].n
    val _n4 = lens[Nested3].n
    val _n5 = lens[Nested4].n
    val _n6 = lens[Nested5].n

    val _n0_i = lens[Nested0].i
    val _n3_i = lens[Nested3].i
    val _n6_i = lens[Nested6].i

    val _n0Ton3I = _n3_i compose _n3 compose _n2 compose _n1
    val _n0Ton6I = _n6_i compose _n6 compose _n5 compose _n4 compose _n3 compose _n2 compose _n1


    override def lensGet0(in: Nested0Input) = _n0_i.get(in.n0)
    override def lensGet3(in: Nested0Input) = _n0Ton3I.get(in.n0)
    override def lensGet6(in: Nested0Input) = _n0Ton6I.get(in.n0)

    override def lensSet0(in: Nested0Input) = _n0_i.set(in.n0)(43)
    override def lensSet3(in: Nested0Input) = _n0Ton3I.set(in.n0)(43)
    override def lensSet6(in: Nested0Input) = _n0Ton6I.set(in.n0)(43)

    override def lensModify0(in: Nested0Input) = _n0_i.modify(in.n0)(_ + 1)
    override def lensModify3(in: Nested0Input) = _n0Ton3I.modify(in.n0)(_ + 1)
    override def lensModify6(in: Nested0Input) = _n0Ton6I.modify(in.n0)(_ + 1)

    override def lensModifyF0(in: Nested0Input): Maybe[Nested0] = ???
    override def lensModifyF3(in: Nested0Input): Maybe[Nested0] = ???
    override def lensModifyF6(in: Nested0Input): Maybe[Nested0] = ???
  }

  object StdLensBench extends LensBench {

    override def lensGet0(in: Nested0Input) = in.n0.i
    override def lensGet3(in: Nested0Input) = in.n0.n.n.n.i
    override def lensGet6(in: Nested0Input) = in.n0.n.n.n.n.n.n.i


    override def lensSet0(in: Nested0Input) = in.n0.copy(i = 43)
    override def lensSet3(in: Nested0Input) = in.n0.copy(n = in.n0.n.copy(n = in.n0.n.n.copy(n = in.n0.n.n.n.copy(i = 43))))
    override def lensSet6(in: Nested0Input) = in.n0.copy(
      n = in.n0.n.copy(
        n = in.n0.n.n.copy(
          n = in.n0.n.n.n.copy(
            n = in.n0.n.n.n.n.copy(
              n = in.n0.n.n.n.n.n.copy(
                n = in.n0.n.n.n.n.n.n.copy(
                  i = 43
                )))))))

    override def lensModify0(in: Nested0Input) = in.n0.copy(i = in.n0.i + 1)
    override def lensModify3(in: Nested0Input) = in.n0.copy(n = in.n0.n.copy(n = in.n0.n.n.copy(n = in.n0.n.n.n.copy(i = in.n0.n.n.n.i + 1))))
    override def lensModify6(in: Nested0Input) = in.n0.copy(
      n = in.n0.n.copy(
        n = in.n0.n.n.copy(
          n = in.n0.n.n.n.copy(
            n = in.n0.n.n.n.n.copy(
              n = in.n0.n.n.n.n.n.copy(
                n = in.n0.n.n.n.n.n.n.copy(
                  i = in.n0.n.n.n.n.n.n.i + 1
                )))))))

    override def lensModifyF0(in: Nested0Input) = safeDivide(in.n0.i, 2).map(_i => in.n0.copy(i = _i))
    override def lensModifyF3(in: Nested0Input) = safeDivide(in.n0.n.n.n.i, 2).map(_i =>
      in.n0.copy(n = in.n0.n.copy(n = in.n0.n.n.copy(n = in.n0.n.n.n.copy(i = _i))))
    )
    override def lensModifyF6(in: Nested0Input) = safeDivide(in.n0.n.n.n.n.n.n.i, 2).map(_i => in.n0.copy(
      n = in.n0.n.copy(
        n = in.n0.n.n.copy(
          n = in.n0.n.n.n.copy(
            n = in.n0.n.n.n.n.copy(
              n = in.n0.n.n.n.n.n.copy(
                n = in.n0.n.n.n.n.n.n.copy(
                  i = _i
                ))))))))
  }

  // ===================================================================================================================

  val input = new Nested0Input // TODO We don't support a suite-wide setup yet.

  sealed abstract class Size(val size: Int)
  case object Size0 extends Size(0)
  case object Size3 extends Size(3)
  case object Size6 extends Size(6)

  sealed trait Op
  case object Get extends Op
  case object Set extends Op
  case object Modify extends Op
  // case object ModifyF extends Op // Shapeless doesn't support

  case class Params(op: Op, size: Size) {
    override def toString = s"$op ${size.size}"
  }

  type B = LensBench => Nested0Input => Any
  val setup = Benchmark.setup[Params, B]{
    case Params(Get, Size0)    => _.lensGet0
    case Params(Get, Size3)    => _.lensGet3
    case Params(Get, Size6)    => _.lensGet6
    case Params(Set, Size0)    => _.lensSet0
    case Params(Set, Size3)    => _.lensSet3
    case Params(Set, Size6)    => _.lensSet6
    case Params(Modify, Size0) => _.lensModify0
    case Params(Modify, Size3) => _.lensModify3
    case Params(Modify, Size6) => _.lensModify6
  }

  def bm(name: String, lb: LensBench): Benchmark[Params] =
    setup.map(_(lb))(name)(f => f(input))

  val suite = Suite[Params]("Lens libraries")(
    bm("No lenses; plain Scala"             , StdLensBench),
    bm(Libraries.Monocle.fullName           , MonocleLensBench),
    bm(Libraries.Monocle.fullName + " macro", MonocleMacroLensBench),
    bm(Libraries.Scalaz.fullName            , ScalazLensBench),
    bm(Libraries.Shapeless.fullName         , ShapelessLensBench))

  val param1 = GuiParam.enumOf[Op]("Op", Get, Set, Modify)(_.toString, initialValues = Seq(Modify))
  val param2 = GuiParam.enumOf[Size]("Size", Size0, Size3, Size6)(_.size.toString, initialValues = Seq(Size6))

  val iso = GenIso.fields[Params]
  val params = GuiParams.combine2(iso)(param1, param2)

  val guiSuite = GuiSuite(suite, params).describe(
    <.div(<.div(^.marginBottom := "0.8em",
      "Comparison of different Lens libraries.",
      <.br,
      "Benchmark taken from ", <.a(^.href := "https://github.com/julien-truffaut/Monocle/tree/master/bench/src/main/scala/monocle/bench", "Monocle"), "."),
      linkToSource(sourceFilename)))
}
