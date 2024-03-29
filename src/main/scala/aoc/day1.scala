package aoc

import aoc.exports.{*, given}

import language.implicitConversions

import spire.math.Numeric
import spire.implicits.given

object Day1:

  def fuel[A: Numeric](mass: A): A =
    N.floor(mass / 3) - 2

  def fuelRec[A: Numeric](mass: A) =
    LazyList.iterate(mass)(fuel).tail.takeWhile(_ > N.zero).sumAll

  def calcFuel[A: Numeric](f: A => A)(as: List[A]) =
    as.map(f).sumAll

  val totalFuel = sourceFile andThen parseInts map calcFuel(fuel)
  val totalFuelRec = sourceFile andThen parseInts map calcFuel(fuelRec)

  val day1_1 = challenge("day1")(totalFuel)
  val day1_2 = challenge("day1")(totalFuelRec)

end Day1
