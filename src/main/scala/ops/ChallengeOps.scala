package ops

import zio._
import zio.blocking._

object ChallengeOps with

  def intChallenge(name: String)(challenge: TaskR[List[String], Int]): URIO[Blocking, Result] =
    val program = for
      input  <- FileIO.lines(s"inputs/$name")
      answer <- challenge.provide(input)
      _      <- FileIO.writeInt(s"solutions/$name", answer)
    yield ()
    program.mapError(pprintThrowable).result
  end intChallenge

  val inputInts: ZIO[List[String], NumberFormatException, List[Int]] =
    ZIO.accessM(in =>
      ZIO.traverse(in.zipWithIndex)((s, i) =>
        StringIO.parseInt(s).mapError(err => NumberFormatException(s"At input ${i + 1}: ${err.getMessage}"))))

  def [E](zio: ZIO[E, String, Unit]) result: URIO[E, Result] = zio.fold(Result.Error(_), _ => Result.Success)

  private def pprintThrowable(err: Throwable) = s"${err.getClass.getSimpleName}: ${err.getMessage}"
