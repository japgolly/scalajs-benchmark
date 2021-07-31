package demo

import monocle.Iso
import scala.deriving._

object TempExt {

  object GenIso {
    transparent inline def fields[A <: Product](using m: Mirror.ProductOf[A]): Iso[A, _ <: Tuple] =
      Iso[A, m.MirroredElemTypes](
        Tuple.fromProductTyped(_))(
        m.fromProduct)

    // transparent inline def fields[A <: Product](using m: Mirror.ProductOf[A]): Iso[A, Any] =
    //   ${ fields[A]('m) }

    // def fields[A <: Product](e: Expr[Mirror.ProductOf[A]])(using Quotes): Expr[Iso[A, Any]] = {
    //   import quotes.reflect._
    //   e match {
    //     case '{ $m: Mirror.ProductOf[A] {type MirroredElemTypes = a *: EmptyTuple} } =>
    //       '{
    //           val f: A => a = Tuple.fromProductTyped(_)(using $m).asInstanceOf[Tuple1[a]]._1
    //           val g: a => A = a => $m.fromProduct(Tuple1(a))
    //         Iso[A, a](f)(g)
    //       }

    //     case '{ type t <: Tuple; $m: Mirror.ProductOf[A] {type MirroredElemTypes = `t`} } =>
    //       '{
    //           val f: A => t = Tuple.fromProductTyped(_)(using $m)
    //           val g: t => A = $m.fromProduct(_)
    //         Iso[A, t](f)(g)
    //       }
    //   }
    // }
  }

}
