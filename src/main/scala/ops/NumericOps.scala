package ops

import language.implicitConversions

import spire.math._
import reflect.ClassTag
import math.pow

object NumericOps:

  def N[A: Numeric] = summon[Numeric[A]]
  def I[A: Integral] = summon[Integral[A]]

  given NumericSyntax: AnyRef with
    extension [A: Numeric](self: A)
      def / (that: A): A        = N.div(self, that)
      def > (that: A): Boolean  = N.gt(self, that)
      def - (that: A): A        = N.minus(self, that)
      def + (that: A): A        = N.plus(self, that)
      def * (that: A): A        = N.times(self, that)
  end NumericSyntax

  given IntegralSyntax: AnyRef with
    extension [A: Integral](self: A)
      def % (that: A): A = I.emod(self, that)
  end IntegralSyntax

  given NumericIterableOps: AnyRef with
    extension [A: Numeric](it: Iterable[A])
      def sumAll: A = it.foldLeft(N.zero)(_ + _)
  end NumericIterableOps

  given [A: Numeric]: Conversion[Int, A] = N.fromInt

  object IntOps:

    def splitDigits[A: Numeric: Integral: ClassTag](i: A): Array[A] =
      digits(i).toArray

    def splitDigits[A: Numeric: Integral: ClassTag](i: A, padLeft: Int): Array[A] =
      digits(i).reverse.padTo[A](padLeft, 0).reverse.toArray

    def collapse[A: Numeric: ClassTag](arr: Array[A]): A =
      arr.reverse.zipWithIndex.map((n, i) => (n * N.fromDouble(pow(10, i.toDouble)))).reverse.foldLeft(N.zero)(_+_)

  end IntOps


  private def digits[A: Numeric: Integral](a: A): List[A] =
    def inner(acc: List[A], a: A): List[A] = a match
      case 0 => acc
      case n => inner(n % 10 :: acc, n / 10)
    if a == 0 then
      0 :: Nil
    else
      inner(Nil, a)
  end digits

end NumericOps
