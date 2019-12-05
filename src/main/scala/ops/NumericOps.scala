package ops

import spire.math._
import math.pow

object NumericOps with

  def N[A: Numeric] = summon[Numeric[A]]

  given NumericSyntax: [A: Numeric](self: A) with
    def / (that: A): A        = N.div(self, that)
    def > (that: A): Boolean  = N.gt(self, that)
    def - (that: A): A        = N.minus(self, that)
    def + (that: A): A        = N.plus(self, that)
  end NumericSyntax

  given NumericIterableOps: [A: Numeric](it: Iterable[A])
    def sumAll: A = it.foldLeft(N.zero)(_ + _)
  end NumericIterableOps

  given [A: Numeric]: Conversion[Int, A] = N.fromInt

  object IntOps with

    def splitDigits(i: Int): Array[Int] =
      digits(i)

    def splitDigits(i: Int, padLeft: Int): Array[Int] =
      digits(i).reverse.padTo(padLeft, 0).reverse

    def collapse(arr: Array[Int]): Int =
      arr.reverse.zipWithIndex.map((n, i) => (n * pow(10, i.toDouble)).toInt).reverse.sum

  end IntOps


  private def digits(a: Int): Array[Int] =
    def inner(acc: List[Int], a: Int): List[Int] = a match
      case 0 => acc
      case n => inner(n % 10 :: acc, n / 10)
    if a == 0 then
      Array(0)
    else
      inner(Nil, a).toArray
  end digits

end NumericOps
