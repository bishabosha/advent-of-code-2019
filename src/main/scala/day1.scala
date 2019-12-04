import language.implicitConversions

import aoc.{_,given}
import spire.math._

object Day1 with

  def fuel[A: Numeric](mass: A): A =
    N.floor(mass / 3) - 2

  def fuelRec[A: Numeric](mass: A) =
    LazyList.iterate(mass)(fuel).tail.takeWhile(_ > N.zero).sumAll

  def calcFuel[A: Numeric](f: A => A)(as: List[A]) =
    as.map(f).sumAll

  val totalFuel    = inputInts.map(calcFuel(fuel))
  val totalFuelRec = inputInts.map(calcFuel(fuelRec))

  val day1_1 = intChallenge("day1")(totalFuel)
  val day1_2 = intChallenge("day1")(totalFuelRec)

end Day1
