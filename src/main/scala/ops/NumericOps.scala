package ops

import spire.math._

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
