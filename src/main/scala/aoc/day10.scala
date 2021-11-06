package aoc

import exports.*

import java.lang.Math.{abs, max, toDegrees, atan2, PI}

import zio._

object Day10:

  given Ordering[Double] = Ordering.Double.TotalOrdering

  type Coords = IArray[IArray[Boolean]]
  type State = List[Coord]

  val map = sourceFile.map(_.map(_.toCharArray.map(_=='#')).toArray.transpose.asInstanceOf[Coords])

  def collectTrue(coords: Coords): State =
    (0 until coords.length).foldLeft(List.empty[Coord])((acc, x) =>
      (0 until coords(0).length).foldLeft(acc)((acc, y) =>
        if coords(x)(y) then Coord(x,y) :: acc else acc))

  inline def searchGen[A](z: => A, f: => (Coord, A) => A)(x: Int, y: Int)(state: State) =
    def hits(acc: A, remaining: State): A = remaining match
    case c::remaining =>
      val theta = angle(x,y)(c)
      hits(f(c,acc), remaining.filter(angle(x,y)(_) != theta))
    case Nil => acc
    hits(z, state.sortBy(dist(x,y)).tail)

  def search(x: Int, y: Int)(state: State) = searchGen(0, (_,acc) => 1 + acc)(x,y)(state)
  def searchAcc(x: Int, y: Int)(state: State) = searchGen(List.empty[Coord], _::_)(x,y)(state)

  def searchAll(state: State): UIO[(Int, Int, Int)] =
    UIO.foreachPar(state)(c => UIO.succeed((c.x,c.y,search(c.x,c.y)(state)))).map(_.maxBy(_._3))

  val mostFound = map >>= (collectTrue andThen (searchAll(_) map (_._3)))

  def mostAt(x: Int, y: Int)(state: State) =
    searchAcc(x,y)(state).groupBy(angle(x,y)).view.mapValues(_.sortBy(-dist(x,y)(_)).head)

  def searchThenMost(index: Int) =
    map map collectTrue >>= (s => searchAll(s) map (p => mostAt(p._1, p._2)(s).toSeq.sortBy(_._1).apply(index)._2))

  def angle(x: Int, y: Int)(a: Coord) =
    toDegrees(atan2((a.y - y).toDouble, (a.x - x).toDouble) + PI/2.0).ensureWhen(_ < 0)(_ + 360)

  def dist(x: Int, y: Int)(a: Coord) = abs(x - a.x) + abs(y - a.y)

  val day10_1 = challenge("day10")(mostFound)
  val day10_2 = challenge("day10")(searchThenMost(200.th))

end Day10
