import language.implicitConversions

import aoc._; import aoc.given
import spire.math._

def fuel[A: Numeric](mass: A): A =
  N.floor(mass / 3) - 2

def fuelRec[A: Numeric](mass: A) =
  LazyList.iterate(mass)(fuel).tail.takeWhile(_ > N.zero).sumAll

def calcFuel[A: Numeric](f: A => A)(as: List[A]) =
  as.map(f).sumAll

val totalFuel    = inputInts.map(calcFuel(fuel))
val totalFuelRec = inputInts.map(calcFuel(fuelRec))

val exit1 = intChallenge(in="day1", out="day1_1")(totalFuel).sync()
val exit2 = intChallenge(in="day1", out="day1_2")(totalFuelRec).sync()
