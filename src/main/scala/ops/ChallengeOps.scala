package ops

import zio._
import zio.blocking._
import zio.console._

object ChallengeOps:

  def challenge[A](in: String)(challenge: RIO[Challenge & Console, A]): UIO[Either[String, A]] =
    (((FileIO lines s"inputs/$in") flatMap ((mklayer(_)) andThen challenge.provideLayer)) mapError pprint)
      .either provideLayer Blocking.live

  def sourceLinesN(n: Int): ZIO[Challenge, IndexOutOfBoundsException, List[String]] =
    sourceFile flatMap (sf => ZIO.effect(sf take n)) refineToOrDie

  val sourceFile = URIO.access[Challenge](_.get.sourceFile)
  val sourceHead = sourceLinesN(1) map (_.head)

  val parseInts = parse(StringIO.parseInt)
  val parseLongs = parse(StringIO.parseLong)

  private def mklayer(lines: List[String]): ULayer[Challenge & Console] =
    Challenge.makeLayer(lines) ++ Console.live

  private def parse[A](f: String => IO[NumberFormatException, A]) =
    URIO.accessM[List[String]](in => ZIO.foreach(in.zipWithIndex)((s, i) =>
      f(s) mapError (err => NumberFormatException(s"At input ${i + 1}: ${err.getMessage}"))))

  private def pprint(err: Throwable) = s"${err.getClass.getSimpleName}: ${err.getMessage}"
