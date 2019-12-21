package aoc

import IntCodes.{getTape => paintProg, State => ProgState, _}

import zio._
import zio.console._

import math._

object Day11 with

  type State = Map[Coord, Long]

  def panels(start: Int) =
    paintProg flatMap (boot => runPaint(initial(boot), Coord(0,0), 0)(Map(Coord(0,0) -> start.toLong)))

  def runPaint(prog: ProgState, current: Coord, dir: Int)(state: State): IO[IllegalStateException, State] =
    IO.fromEither(concurrent(prog.copy(in=LazyList.continually(state.getOrElse(current, 0))))).flatMap {
      case Suspend.Yield(prog)  => IO.effectSuspendTotal(runPaint1(prog,current,dir)(state+(current->prog.out.head)))
      case Suspend.Terminate(_) => IO.succeed(state)
    }

  def runPaint1(prog: ProgState, current: Coord, facing: Int)(state: State): IO[IllegalStateException, State] =
    IO.fromEither(concurrent(prog)).flatMap {
      case Suspend.Yield(prog) =>
        val dir = ((facing + (if prog.out.head == 0 then -90 else 90)) % 360).adjust(_ < 0)(_+360)
        val next = Coord(
          x = current.x + sin(toRadians(dir.toDouble)).toInt,
          y = current.y - cos(toRadians(dir.toDouble)).toInt
        )
        IO.effectSuspendTotal(runPaint(prog.copy(out=Nil),next,dir)(state))
      case Suspend.Terminate(_) => IO.fail(IllegalStateException("unexpected exit"))
    }

  def numberOfPanels(start: Int) = panels(start) map (_.size)

  def message(start: Int) = messageRows(start) map { s =>
    val length = (s map (_.keys.max)).max
    s.map(xs => (0 to length) map (i => if xs.contains(i) then '*' else ' ') mkString).mkString("\n","\n","")
  }

  def messageRows(start: Int) =
    panels(start) map (_.groupMap(_._1.y)((coord, c) => coord.x -> c).view
      .mapValues(_.toSeq sortBy(_._1) filter(_._2 == 1)).toSeq.sortBy(_._1).map(_._2.toMap))

  val day11_1 = challenge("day11")(numberOfPanels(0))
  val day11_2 = challenge("day11")(message(1))

end Day11
