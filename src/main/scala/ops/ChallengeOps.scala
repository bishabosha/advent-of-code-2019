package ops

import zio._
import zio.blocking._
import zio.console._

object ChallengeOps with

  def intChallenge(in: String)(challenge: TaskR[List[String], Int]): URIO[Blocking, Either[String, Int]] =
    val program = for
      input  <- FileIO.lines(s"inputs/$in")
      answer <- challenge.provide(input)
    yield answer
    program.mapError(pprintThrowable).either
  end intChallenge

  def stringChallenge(in: String)(challenge: TaskR[List[String], String]): URIO[Blocking, Either[String, String]] =
    val program = for
      input  <- FileIO.lines(s"inputs/$in")
      answer <- challenge.provide(input)
    yield answer
    program.mapError(pprintThrowable).either
  end stringChallenge

  def inputLines(n: Int): ZIO[List[String], IndexOutOfBoundsException, List[String]] =
    ZIO.accessM(xs => ZIO.effect(xs.take(n)).refineToOrDie)

  val inputLine: ZIO[List[String], IndexOutOfBoundsException, String] =
    inputLines(1).map(_.head)

  val inputInts: ZIO[List[String], NumberFormatException, List[Int]] =
    ZIO.accessM(in =>
      ZIO.traverse(in.zipWithIndex)((s, i) =>
        StringIO.parseInt(s).mapError(err => NumberFormatException(s"At input ${i + 1}: ${err.getMessage}"))))

  private def pprintThrowable(err: Throwable) = s"${err.getClass.getSimpleName}: ${err.getMessage}"
