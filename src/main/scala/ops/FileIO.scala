package ops

import java.nio.file._
import scala.jdk.CollectionConverters._
import java.io.IOException

import zio._
import zio.blocking._

object FileIO with

  def lines(file: String): ZIO[Blocking, IOException, List[String]] =
    for
      path  <- ZIO.succeed(Paths.get(file))
      lines <- effectBlocking(Files.readAllLines(path).asScala.toList).refineToOrDie[IOException]
    yield
      lines

  def writeInt(file: String, int: Int): ZIO[Blocking, IOException, Unit] =
    for
      path <- ZIO.succeed(Paths.get(file))
      _    <- effectBlocking(Files.write(path, int.toString.getBytes)).refineToOrDie[IOException]
    yield ()
