package aoc

import IntCodes._
import zio._

object Day9

  def BOOST(testIn: Int) =
    getTape.map(tpe =>
      nonconcurrent(initial(tpe, testIn::Nil)).map(_.out.headOption)
    ) >>= IO.fromEither someOrFailException

  val day9_1 = challenge("day9")(BOOST(1))
  val day9_2 = challenge("day9")(BOOST(2))

end Day9
