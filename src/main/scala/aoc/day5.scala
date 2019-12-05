package aoc

import zio._
import IntCodes._

object Day5 with

  def run(in: List[Int]) = (getTape >>= (tpe => ZIO.fromEither(exec(tpe,in)))) map (_._3.head)

  val day5_1 = intChallenge("day5")(run(List(1)))
  val day5_2 = intChallenge("day5")(run(List(5)))

end Day5
