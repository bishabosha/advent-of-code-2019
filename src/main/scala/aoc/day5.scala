package aoc

import zio._
import IntCodes._

object Day5 with

  def run(in: List[Int]) = (getTape >>= (tpe => ZIO.fromEither(exec(given initial(tpe,in))))) map (_.out.head)

  val day5_1 = challenge("day5")(run(List(1)))
  val day5_2 = challenge("day5")(run(List(5)))

end Day5
