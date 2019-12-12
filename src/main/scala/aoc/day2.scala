package aoc

import zio._
import PartialFunction.condOpt

import IntCodes._
import Option.when

object Day2 with

  def initialise(noun: Int, verb: Int) =
    RIO.accessM((tape: IArray[Long]) =>
      Task.effect(tape.updated(1, noun.toLong).updated(2, verb.toLong)))

  def find(n: Int, v: Int)(goal: Int) =
    run(n,v).fold(none, x => when(x == goal)(f"$n%2d$v%2d"))

  def search(goal: Int, domain: Range) =
    ZIO.foreach_(domain)(x => ZIO.foreach_(domain)(y => find(x,y)(goal).some.flip)).flip.asError(emptyResult)

  def run(noun: Int, verb: Int) =
    (initialise(noun, verb) >>= (tpe => ZIO.fromEither(nonconcurrent(initial(tpe))))) map (_.mem(0))

  val day2_1 = challenge("day2")(run(noun=12, verb=2) `compose` getTape)
  val day2_2 = challenge("day2")(search(19690720, 0 to 99) `compose` getTape)

end Day2
