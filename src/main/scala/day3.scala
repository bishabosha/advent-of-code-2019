import aoc.{_, given}
import zio._

object Day3 with

  type Coord = (Int,Int)
  type Path  = Array[(Int, Coord)]

  val Op = raw"([R|L|U|D])(\d+)".r

  def offset(old: Coord, current: Int)(dir: Char, dist: Int): (Coord, Int, Path) =
    val x -> y = old
    dir match
    case 'U' => (x -> (y + dist), current + dist, (1 to dist).toArray.map(i => (current + i, x -> (y + i))))
    case 'D' => (x -> (y - dist), current + dist, (1 to dist).toArray.map(i => (current + i, x -> (y - i))))
    case 'L' => ((x - dist) -> y, current + dist, (1 to dist).toArray.map(i => (current + i, (x - i) -> y)))
    case 'R' => ((x + dist) -> y, current + dist, (1 to dist).toArray.map(i => (current + i, (x + i) -> y)))

  def state(old: Coord, current: Int)(in: String): Either[String, (Coord, Int, Path)] = in match
    case Op(op, dist) => Right(offset(old, current)(op.head, dist.toInt))
    case _            => Left(s"malformatted input $in")

  def parse(xs: List[String]) =
    ZIO.foldLeft(xs)(((0,0), 0, Array.empty[(Int, Coord)])::Nil)({ (acc, s) =>
      val (old, current, _) :: rest = acc
      ZIO.fromEither(state(old, current)(s))
         .map(_ :: acc)
         .mapError(IllegalArgumentException(_))
    })

  def parsePath(xs: List[String]) = parse(xs).map(_.map(_._3).toArray.flatMap(_.map(_._2)))
  def parsePathWithSteps(xs: List[String]) = parse(xs).map(_.map(_._3).toArray.flatten)

  val getCoords = inputLines(2) >>= (ZIO.foreachPar(_)(parsePath `compose` (_.split(',').toList)))
  val getCoordsWithSteps = inputLines(2) >>= (ZIO.foreachPar(_)(parsePathWithSteps `compose` (_.split(',').toList)))

  val shortestDist =
    for
      crosses   <- getCoords.map(_.map(_.toSet).reduce(_ `intersect` _) - ((0,0)))
      distances =  crosses.map((x,y) => math.abs(x) + math.abs(y)).toList.sortWith(_ < _)
      shortest  <- ZIO.effect(distances.head)
    yield
      shortest

  val shortestDistWithSteps =
    for
      coordss    <- getCoordsWithSteps
      stepCounts =  coordss.map(_.groupBy(_._2).view.mapValues(_.map(_._1).sortWith(_ < _).head))
      crosses    =  coordss.map(_.map(_._2).toSet).reduce(_ `intersect` _) - ((0,0))
      distances  =  crosses.map(xy => stepCounts.map(_(xy)).sum).toList.sortWith(_ < _)
      shortest   <- ZIO.effect(distances.head)
    yield
      shortest

  val day3_1 = intChallenge(in="day3",out="day3_1")(shortestDist)
  val day3_2 = intChallenge(in="day3",out="day3_2")(shortestDistWithSteps)

end Day3
