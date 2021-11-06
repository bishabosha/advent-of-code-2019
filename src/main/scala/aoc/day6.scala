package aoc

import exports.*

import zio._

object Day6:

  type State = Map[String, String]

  val Orbit = raw"(\w+?)\)(\w+)".r

  val parseOrbits: RIO[Challenge, State] =
    sourceFile >>= (IO.foldLeft(_)(Map.empty)({ (acc, s) => s match
      case Orbit(a,b) => IO.succeed(acc.updated(b,a))
      case _          => IO.fail(IllegalArgumentException(s"Illegal input: $s"))
    }))

  val numberOfOrbits = parseOrbits `map` (sumOrbits(using _))
  def numberOfHops(src: String, dest: String) = parseOrbits `map` (hops(src, dest)(using _))

  def sumOrbits(using State) = state.keys.view.map(depth).sum

  inline def traverse[A](empty: => A)(combine: => (String, A) => A)(obj: String)(using State): A =
    def links(acc: A, key: String): A =
      state.get(key) match
      case Some(link) => links(combine(link, acc), link)
      case None       => acc
    links(empty, obj)

  def depth(obj: String)(using State) = traverse(0)((_,d) => d+1)(obj)
  def path(obj: String)(using State) = traverse(Nil: List[String])(_::_)(obj)

  def hops(you: String, dest: String)(using State) =
    val youP = path(you)
    val destP = path(dest)
    val prefix = (youP `zip` destP `takeWhile` (_==_)).length
    (youP.length - prefix) + (destP.length - prefix)
  end hops

  val day6_1 = challenge("day6")(numberOfOrbits)
  val day6_2 = challenge("day6")(numberOfHops("YOU", "SAN"))

  inline def state(using state: State): state.type = state

end Day6
