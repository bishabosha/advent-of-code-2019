import aoc.{_, given}
import zio._

object Day3 with

  type Coord = (Int,Int)
  type Path  = Array[Coord]

  val Op = raw"([R|L|U|D])(\d+)".r

  def offset(old: Coord)(dir: Char, dist: Int): (Coord, Path) =
    val x -> y = old
    dir match
    case 'U' => (x -> (y + dist), (1 to dist).toArray.map(i => x -> (y + i)))
    case 'D' => (x -> (y - dist), (1 to dist).toArray.map(i => x -> (y - i)))
    case 'L' => ((x - dist) -> y, (1 to dist).toArray.map(i => (x - i) -> y))
    case 'R' => ((x + dist) -> y, (1 to dist).toArray.map(i => (x + i) -> y))

  def state(old: Coord)(in: String): Either[String, (Coord, Path)] = in match
    case Op(op, dist) => Right(offset(old)(op.head, dist.toInt))
    case _            => Left(s"malformatted input $in")

  def parse(xs: List[String]) =
    ZIO.foldLeft(xs)(((0,0), Array.empty[Coord])::Nil)((acc, s) =>
      ZIO.fromEither(state(acc.head._1)(s))
         .map(_ :: acc)
         .mapError(IllegalArgumentException(_)))

  def parsePath(xs: List[String]) = parse(xs).map(_.map(_._2).toArray.flatten)

  val getCoords = inputLines(2) >>= (ZIO.foreachPar(_)(parsePath `compose` (_.split(',').toList)))

  val shortestDist =
    for
      crosses   <- getCoords.map(_.map(_.toSet).reduce(_ `intersect` _) - ((0,0)))
      distances =  crosses.map((x,y) => math.abs(x) + math.abs(y)).toList.sortWith(_ < _)
      shortest  <- ZIO.effect(distances.head)
    yield
      shortest

  val day3_1 = intChallenge(in="day3",out="day3_1")(shortestDist)

end Day3
