package ops

import zio._
import zio.blocking._
import zio.console._

object ChallengeOps with

  def intChallenge(inout: String)(challenge: TaskR[List[String], Int]): URIO[Blocking, Result] =
    intChallenge(inout, inout)(challenge)

  def intChallenge(in: String, out: String)(challenge: TaskR[List[String], Int]): URIO[Blocking, Result] =
    val program = for
      input  <- FileIO.lines(s"inputs/$in")
      answer <- challenge.provide(input)
      _      <- FileIO.writeInt(s"solutions/$out", answer)
    yield ()
    program.mapError(pprintThrowable).result
  end intChallenge

  def stringChallenge(in: String, out: String)(challenge: TaskR[List[String], String]): URIO[Blocking, Result] =
    val program = for
      input  <- FileIO.lines(s"inputs/$in")
      answer <- challenge.provide(input)
      _      <- FileIO.writeString(s"solutions/$out", answer)
    yield ()
    program.mapError(pprintThrowable).result
  end stringChallenge

  def inputLines(n: Int): ZIO[List[String], IndexOutOfBoundsException, List[String]] =
    ZIO.accessM(xs => ZIO.effect(xs.take(n)).refineToOrDie)

  val inputLine: ZIO[List[String], IndexOutOfBoundsException, String] =
    inputLines(1).map(_.head)

  val inputInts: ZIO[List[String], NumberFormatException, List[Int]] =
    ZIO.accessM(in =>
      ZIO.traverse(in.zipWithIndex)((s, i) =>
        StringIO.parseInt(s).mapError(err => NumberFormatException(s"At input ${i + 1}: ${err.getMessage}"))))

  def [E](zio: ZIO[E, String, Unit]) result: URIO[E, Result] = zio.fold(Result.Error(_), _ => Result.Success)

  private def pprintThrowable(err: Throwable) = s"${err.getClass.getSimpleName}: ${err.getMessage}"
