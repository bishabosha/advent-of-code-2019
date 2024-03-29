package aoc

import aoc.exports.*
import zio.*

import spire.implicits.given

object Day12:

  type State = IndexedSeq[Moon]

  final case class Moon(x: Long, y: Long, z: Long, xV: Long, yV: Long, zV: Long)

  extension (m: Moon) def transpose = Array(Array(m.x, m.xV), Array(m.y, m.yV), Array(m.z, m.zV))

  val Position = raw"<x=(-?\d+?),\s*?y=(-?\d+?),\s*z=(-?\d+)>".r

  val moons = sourceFile flatMap (IO.foreach(_) {
    case Position(x, y, z) => IO.succeed(Moon(x.toLong, y.toLong, z.toLong, 0, 0, 0))
    case fail              => IO.fail(IllegalArgumentException(s"not a position: $fail"))
  })

  def run(steps: Int, state: State) = (LazyList.iterate(state)(step).tail take steps).last

  def step(state: State): State = (
    for i <- state.indices.toArray
    yield applyVelocity(state.indices.view.filter(_ != i).foldLeft(state(i))((m, n) => update(m, state(n))))
  ).toIndexedSeq

  extension (moons: State) def toAxess = ((moons map (_.transpose)).transpose) map (_.flatten)

  def applyVelocity(m: Moon) = m.copy(x = m.x + m.xV, y = m.y + m.yV, z = m.z + m.zV)

  def update(m: Moon, n: Moon) = m.copy(
    xV = m.xV - (m.x - n.x).sign,
    yV = m.yV - (m.y - n.y).sign,
    zV = m.zV - (m.z - n.z).sign
  )

  def energy(m: Moon) = m match
    case Moon(x, y, z, xV, yV, zV) => (x.abs + y.abs + z.abs) * (xV.abs + yV.abs + zV.abs)

  def totalEnergy(state: State) = (state map energy).sum
  def totalEnergyAfter(steps: Int) = moons map (ms => run(steps, ms.toArray.toIndexedSeq)) map totalEnergy
  val firstRepetition = moons map (ms => searchRepetitions(ms.toArray.toIndexedSeq))

  def searchRepetitions(moons: State) =
    val first = moons.toAxess

    def inner(i: Int, moons: State, periods: Array[Int]): Long =
      val periods1 = periods lazyZip first lazyZip moons.toAxess map ((o, orig, curr) =>
        o.ensureWhen(-1 == _ && curr == orig)(_ => i)
      )
      if periods1 exists (-1 == _) then inner(i + 1, step(moons), periods1)
      else periods1 map (_.toLong) reduce I.lcm

    inner(1, step(moons), Array.fill(3)(-1))

  val day12_1 = challenge("day12")(totalEnergyAfter(1000))
  val day12_2 = challenge("day12")(firstRepetition)

end Day12
