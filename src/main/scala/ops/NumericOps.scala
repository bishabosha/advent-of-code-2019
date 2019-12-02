package ops

import spire.math._

object NumericOps with

  def N[A: Numeric] = summon[Numeric[A]]

  given [A: Numeric]: Conversion[Int, A] = N.fromInt
