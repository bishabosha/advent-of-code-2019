import language.implicitConversions

import aoc._; import aoc.given
import spire.math._

def fuel[A: Numeric](mass: A) =
  N.minus(N.floor(N.div(mass, 3)), 2)

def fuelRec[A: Numeric](mass: A) =
  LazyList.iterate(mass)(fuel).tail.takeWhile(N.gt(_, N.zero)).foldLeft(N.zero)(N.plus)

def calcFuel[A: Numeric](f: A => A)(as: List[A]) =
  as.map(f).foldLeft(N.zero)(N.plus)

val totalFuel    = inputInts.map(calcFuel(fuel))
val totalFuelRec = inputInts.map(calcFuel(fuelRec))

val exit1 = intChallenge("day1_1")(totalFuel).sync()
val exit2 = intChallenge(in="day1_1", out="day1_2")(totalFuelRec).sync()
