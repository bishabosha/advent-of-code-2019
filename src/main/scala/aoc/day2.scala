package aoc

import zio._
import PartialFunction.condOpt

import IntCodes._
import Option.when

object Day2 with

  def tabulate(range: Range) = range.map(n => range.map(n -> _)).flatten

  def initialise(noun: Int, verb: Int) =
    RIO.accessM((tape: IArray[Int]) =>
      Task.effect(tape.updated(1, noun).updated(2, verb)))

  def find(n: Int, v: Int)(goal: Int) =
    run(n,v).fold(none, x => when(x == goal)(f"$n%2d$v%2d"))

  def search(goal: Int, size: Range) =
    ZIO.foreach(tabulate(size))(find(_,_)(goal).some.flip).flip.asError(emptyResult)

  def run(noun: Int, verb: Int) =
    (initialise(noun, verb) >>= (tpe => ZIO.fromEither(exec(given initial(tpe,Nil))))) map (_.mem(0))

  val day2_1 = challenge("day2")(run(noun=12, verb=2) `compose` getTape)
  val day2_2 = challenge("day2")(search(19690720, 0 to 99) `compose` getTape)

end Day2
