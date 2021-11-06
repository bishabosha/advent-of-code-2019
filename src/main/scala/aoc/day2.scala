package aoc

import aoc.exports.*
import zio.*

import IntCodes.*
import PartialFunction.condOpt

object Day2:

  def initialise(noun: Int, verb: Int): RIO[IArray[Long], IArray[Long]] =
    RIO.accessM(tape => Task.effect(tape.updated(1, noun.toLong).updated(2, verb.toLong)))

  def find(n: Int, v: Int)(goal: Int) =
    run(n, v).fold(none, x => Option.when(x == goal)(f"$n%2d$v%2d"))

  def search(goal: Int, domain: Range) =
    val search = ZIO.foreach_(domain.cartesian)(find(_, _)(goal).some.flip)
    search.flip.mapError(emptyResult)

  def run(noun: Int, verb: Int) =
    (initialise(noun, verb) flatMap (tpe => ZIO.fromEither(nonconcurrent(initial(tpe))))) map (_.mem(0))

  val day2_1 = challenge("day2")(getTape andThen run(noun = 12, verb = 2))
  val day2_2 = challenge("day2")(getTape andThen search(19690720, 0 to 99))

end Day2
