package aoc

import aoc.exports.*
import zio.*

import IntCodes.*

object Day9:

  def boost(testIn: Int) =
    getTape.map(tpe =>
      nonconcurrent(initial(tpe, testIn)).map(_.out.headOption)
    ) flatMap IO.fromEither someOrFailException

  val day9_1 = challenge("day9")(boost(1))
  val day9_2 = challenge("day9")(boost(2))

end Day9
