package aoc

import zio._
import PartialFunction.condOpt

object Day6 with

  type State = Map[String, String]

  val Orbit = raw"(\w+?)\)(\w+)".r

  def parseOrbits(lines: List[String]) =
    IO.foldLeft(lines)(Map.empty[String, String])((acc, s) =>
      IO.fromOption(condOpt(s) { case Orbit(a,b) => acc.updated(b,a) })
        .asError(IllegalArgumentException(s"Illegal input: $s")))

  val orbits = inputLines >>= parseOrbits

  val numberOfOrbits = orbits.map(sumOrbits)
  def numberOfHops(src: String, dest: String) = orbits.map(hops(src, dest))

  def sumOrbits(state: State) =
    state.keys.view.map(path(_)(state).length).sum

  def path(obj: String)(state: State) =
    def links(acc: List[String], key: String): List[String] =
      state.get(key) match
      case Some(link) => links(link :: acc, link)
      case None       => acc
    links(Nil, obj)

  def hops(you: String, dest: String)(state: State) =
    val youP = path(you)(state)
    val destP = path(dest)(state)
    val prefix = youP.zip(destP).takeWhile(_ == _).length
    (youP.length - prefix) + (destP.length - prefix)

  val day6_1 = challenge("day6")(numberOfOrbits)
  val day6_2 = challenge("day6")(numberOfHops("YOU", "SAN"))

end Day6
