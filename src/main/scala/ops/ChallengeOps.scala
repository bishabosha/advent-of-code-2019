package ops

import zio._
import zio.blocking._
import zio.console._

object ChallengeOps with

  private final class ChallengeEnv(sourceFile: List[String]) extends Console.Live with Challenge.Live(sourceFile)

  def challenge[A](in: String)(challenge: RIO[Challenge & Console, A]): UIO[Either[String, A]] =
    ((FileIO lines s"inputs/$in" >>= challenge.provide compose (ChallengeEnv(_))) mapError pprint)
      .either provide Blocking.Live

  def sourceLinesN(n: Int): ZIO[Challenge, IndexOutOfBoundsException, List[String]] =
    sourceFile >>= (sf => ZIO.effect(sf take n)) refineToOrDie

  val sourceFile = URIO.access[Challenge](_.challenge.sourceFile)
  val sourceHead = sourceLinesN(1) map (_.head)

  val parseInts = parse(StringIO.parseInt)
  val parseLongs = parse(StringIO.parseLong)

  private def parse[A](f: String => IO[NumberFormatException, A]) =
    URIO.accessM[List[String]](in => ZIO.traverse(in.zipWithIndex)((s, i) =>
      f(s) mapError (err => NumberFormatException(s"At input ${i + 1}: ${err.getMessage}"))))

  private def pprint(err: Throwable) = s"${err.getClass.getSimpleName}: ${err.getMessage}"
