package ops

import zio._
import zio.blocking._
import zio.console._

object ChallengeOps with

  def challenge[A](in: String)(challenge: TaskR[List[String], A]): URIO[Blocking, Either[String, A]] =
    (FileIO lines s"inputs/$in" >>= challenge.provide) mapError pprintThrowable either

  def inputLinesN(n: Int): ZIO[List[String], IndexOutOfBoundsException, List[String]] =
    ZIO.accessM(xs => ZIO.effect(xs take n) refineToOrDie)

  val inputLine: ZIO[List[String], IndexOutOfBoundsException, String] =
    inputLinesN(1) map (_.head)

  val inputInts: ZIO[List[String], NumberFormatException, List[Int]] =
    ZIO.accessM(in =>
      ZIO.traverse(in.zipWithIndex)((s, i) =>
        StringIO parseInt(s) mapError (err => NumberFormatException(s"At input ${i + 1}: ${err.getMessage}"))))

  private def pprintThrowable(err: Throwable) = s"${err.getClass.getSimpleName}: ${err.getMessage}"
