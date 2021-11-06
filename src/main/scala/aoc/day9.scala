package aoc

import exports.*

import IntCodes._
import zio._

object Day9:

  def boost(testIn: Int) =
    getTape.map(tpe =>
      nonconcurrent(initial(tpe, testIn)).map(_.out.headOption)
    ) >>= IO.fromEither someOrFailException

  val day9_1 = challenge("day9")(boost(1))
  val day9_2 = challenge("day9")(boost(2))

end Day9
