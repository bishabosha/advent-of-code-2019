package aoc

import language.implicitConversions

import zio._

import IntCodes.{getTape => arcadeGame, State => GameState, _}

object Day13 with
  import TileId._

  sealed trait TileId(val id: Int) extends Product with Serializable derives Eql // change to Enum once https://github.com/lampepfl/dotty/issues/7410 is fixed

  object TileId with

    val tiles = Array(Empty, Wall, Block, Paddle, Ball).ensuring(_.zipWithIndex.forall(_.id ==_))

    case object Empty extends TileId(0)
    case object Wall extends TileId(1)
    case object Block extends TileId(2)
    case object Paddle extends TileId(3)
    case object Ball extends TileId(4)

    def tile(id: Long) =
      if id < tiles.length then Right(tiles(id.toInt)) else Left(IllegalStateException(s"unexpected tile id: $id"))

  val empty = CoordL(-1,-1)

  type Screen = Map[CoordL, TileId]

  def TileE(id: Long, x: Long, y: Long) = TileId.tile(id).map(CoordL(x,y) -> _)

  val numberOfBlockTiles =
    (arcadeGame >>= (mem => run(Map.empty,initial(mem)))) map countBlocks

  val finalScore =
    (arcadeGame >>= (mem => IO.fromEither(play(Map.empty,initial(mem.updated(0,2)), -1, empty, empty))))

  def countBlocks(screen: Screen) = screen.filter(_._2 == TileId.Block).size

  def run(screen: Screen, game: GameState): IO[IllegalStateException, Screen] =
    IO.fromEither(nonconcurrent(game)) >>= { terminal =>
      IO.foldLeft(terminal.out.grouped(3).to(Iterable))(Map.empty: Screen) { (sc, point) =>
        val id::y::x::_ = point
        IO.fromEither(TileE(id,x,y)).map(sc + _)
      }
    }

  def play(screen: Screen, game: GameState, score: Long, ball: CoordL, paddle: CoordL): Either[IllegalStateException, Long] =
    concurrent(game) match
    case Right(Suspend.Yield(game)) if game.out.size == 3 =>
      (game.out: @unchecked) match
      case score :: 0 :: -1 :: _ => play(screen, game.copy(out=Nil), score, ball, paddle)
      case id    :: y :: x  :: _ =>
        TileE(id,x,y) match
        case Right(tile @ (pos, id)) =>
          val screen1 = screen + tile
          val ball1   = if id == Ball   then pos else ball
          val paddle1 = if id == Paddle then pos else paddle
          val dir     = (ball1.x compare paddle1.x).sign
          play(screen1, game.copy(out=Nil,in=LazyList.continually(dir.toLong)), score, ball1, paddle1)
        case Left(e) => Left(e)
    case Right(Suspend.Yield(game))  => play(screen, game, score, ball, paddle)
    case Right(Suspend.Terminate(_)) =>
      if countBlocks(screen) == 0 then Right(score)
      else Left(IllegalStateException("Game terminated with some blocks"))
    case Left(e)                     => Left(e)

  val day13_1 = challenge("day13")(numberOfBlockTiles)
  val day13_2 = challenge("day13")(finalScore)

end Day13
