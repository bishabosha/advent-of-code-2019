package aoc

import aoc.exports.*
import zio.*

import IntCodes.*

object Day5:

  def run(in: Int) =
    (getTape flatMap (tpe => ZIO.fromEither(nonconcurrent(initial(tpe, in))))) map (_.out.head)

  val day5_1 = challenge("day5")(run(1))
  val day5_2 = challenge("day5")(run(5))

end Day5
