package aoc

import zio._
import PartialFunction.condOpt

import IntCodes._

object Day2 with

  def tabulate(range: Range) = range.map(n => range.map(n -> _)).flatten

  def search(goal: Int, size: Range) =
    def findFirst(options: List[Option[(Int, Int)]]) =
      options.collectFirst { case Some(n,v) => f"$n%2d$v%2d" }
    for
      options <- ZIO.foreach(tabulate(size))((n, v) =>
                    prog(n, v).fold(_ => None, condOpt(_)({ case x if x == goal => (n, v) })))
      result  <- ZIO.fromOption(findFirst(options)).mapError(_ => IllegalArgumentException("No result found"))
    yield
      result

  def prog(noun: Int, verb: Int) = for
    tape  <- initialise(noun, verb)
    state <- ZIO.fromEither(exec(tape, Nil, Nil))
    first <- ZIO.effect(state._1.head)
  yield first

  val day2_1 = intChallenge("day2")(prog(noun=12, verb=2) `compose` getTape)
  val day2_2 = stringChallenge("day2")(search(19690720, 0 to 99) `compose` getTape)

end Day2
