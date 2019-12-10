package aoc

import zio._

object Day3 with

  type Coord = (Int,Int)
  type Path  = Array[(Int, Coord)]

  val Op = raw"([R|L|U|D])(\d+)".r

  val Steps = Map(
    'U' -> step(u),
    'D' -> step(d),
    'L' -> step(l),
    'R' -> step(r)
  )

  def offset(old: Coord, current: Int, dir: Char, dist: Int): (Coord, Int, Path) =
    Steps(dir)(dist, current, old)

  def step(op: Coord => Int => Coord)(delta: Int, current: Int, coord: Coord) =
    (op(coord)(delta), current + delta, fill(delta, current, coord)(op))

  def fill(delta: Int, current: Int, coord: Coord)(op: Coord => Int => Coord) =
    (1 to delta).toArray.map(i => (current + i, op(coord)(i)))

  def u(x: Int, y: Int)(delta: Int) = x -> (y + delta)
  def d(x: Int, y: Int)(delta: Int) = x -> (y - delta)
  def l(x: Int, y: Int)(delta: Int) = (x - delta) -> y
  def r(x: Int, y: Int)(delta: Int) = (x + delta) -> y

  def state(old: Coord, current: Int)(in: String): Either[String, (Coord, Int, Path)] = in match
    case Op(op, dist) => Right(offset(old, current, op.head, dist.toInt))
    case _            => Left(s"malformatted input $in")

  def parse(xs: List[String]) =
    ZIO.foldLeft(xs)(((0,0), 0, Array.empty[(Int, Coord)])::Nil)({ (acc, s) =>
      val (old, current, _) :: rest = acc
      ZIO.fromEither(state(old, current)(s))
         .map(_ :: acc)
         .mapError(IllegalArgumentException(_))
    }).map(_.map(_._3).toArray)

  def parsePath(xs: List[String]) = parse(xs).map(_.flatMap(_.map(_._2)))
  def parsePathWithSteps(xs: List[String]) = parse(xs).map(_.flatten)

  val getCoords = sourceLinesN(2) >>= (ZIO.foreachPar(_)(parsePath `compose` splitCsv))
  val getCoordsWithSteps = sourceLinesN(2) >>= (ZIO.foreachPar(_)(parsePathWithSteps `compose` splitCsv))

  val shortestDist =
    for
      crosses   <- getCoords.map(_.map(_.toSet).reduce(_ `intersect` _) - ((0,0)))
      distances =  crosses.map((x,y) => math.abs(x) + math.abs(y)).toList.sortWith(_<_)
      shortest  <- ZIO.effect(distances.head)
    yield
      shortest

  val shortestDistWithSteps =
    for
      coordss    <- getCoordsWithSteps
      stepCounts =  coordss map(_.groupBy(_._2).view.mapValues(_.map(_._1) sortWith(_<_) head))
      crosses    =  (coordss map (_.map(_._2) toSet) reduce(_`intersect`_)) - ((0,0))
      distances  =  (crosses map(xy => stepCounts map(_(xy)) sum) toList) sortWith(_<_)
      shortest   <- ZIO.effect(distances.head)
    yield
      shortest

  val day3_1 = challenge("day3")(shortestDist)
  val day3_2 = challenge("day3")(shortestDistWithSteps)

end Day3
