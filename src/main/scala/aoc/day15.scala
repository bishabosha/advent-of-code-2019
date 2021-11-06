package aoc

import aoc.exports.*
import zio.*
import zio.console.*

import IntCodes.{getTape as robot, State as RobotState, *}

object Day15:
  import Direction.*, Response.*

  // change to Enum once https://github.com/lampepfl/dotty/issues/7410 is fixed
  sealed trait Direction(val id: Long) extends Product with Serializable derives CanEqual

  object Direction:

    case object North extends Direction(1)
    case object South extends Direction(2)
    case object West extends Direction(3)
    case object East extends Direction(4)

  // extension (dir: Direction) def inverse =
  //   dir match
  //   case North => South
  //   case South => North
  //   case East  => West
  //   case West  => East
  // end inverse

  end Direction

  def vector(dir: Direction): Coord = dir match
    case North => Coord(0, -1)
    case South => Coord(0, 1)
    case East  => Coord(-1, 0)
    case West  => Coord(1, 0)

  // change to Enum once https://github.com/lampepfl/dotty/issues/7410 is fixed
  sealed trait Response(val id: Int) extends Product with Serializable derives CanEqual

  object Response:

    val responses = Array(Wall, Success, Goal).ensuring(_.zipWithIndex.forall(_.id == _))

    case object Wall extends Response(0)
    case object Success extends Response(1)
    case object Goal extends Response(2)

    def response(id: Long) =
      if id < responses.length then Right(responses(id.toInt))
      else Left(IllegalStateException(s"unexpected response id: $id"))

  end Response

  def explore(robot: RobotState, curr: Coord): ZIO[Console, IllegalStateException, Nothing] =
    IO.fromEither(concurrent(robot)) flatMap {
      case Suspend.Yield(robot) =>
        IO.fromEither(response(robot.out.head)) flatMap {
          case Wall    => UIO.never
          case Success => UIO.never
          case Goal    => UIO.never
        }
      case Suspend.Terminate(_) => IO.fail(IllegalStateException("unexpected terminate"))
    }

  val day15_1 = challenge("day15")(robot flatMap (mem => explore(initial(mem, North.id.toInt), Coord(0, 0))))
// val day15_2 = challenge("day15")

end Day15
