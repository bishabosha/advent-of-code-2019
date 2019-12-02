import language.implicitConversions

import aoc._; import aoc.given
import spire.math._

def fuel[A: Numeric](module: A) = N.minus(N.floor(N.div(module, 3)), 2)

val totalFuel = inputInts.map(_.map(fuel).reduce(N.plus))

val exit = intChallenge("day1_1")(totalFuel).sync()
