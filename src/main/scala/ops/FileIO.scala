package ops

import java.nio.file._
import scala.jdk.CollectionConverters._
import java.io.IOException

import zio._
import zio.blocking._

object FileIO with

  def path(file: String): IO[InvalidPathException, Path] =
    ZIO.effect(Paths.get(file)).refineToOrDie

  def lines(path: Path): ZIO[Blocking, IOException, List[String]] =
    effectBlocking(Files.readAllLines(path).asScala.toList).refineToOrDie

  def writeString(path: Path, str: String): ZIO[Blocking, IOException, Unit] =
    effectBlocking(Files.write(path, str.getBytes)).unit.refineToOrDie

  def lines(file: String): ZIO[Blocking, IOException | InvalidPathException, List[String]] =
    FileIO.path(file) >>= FileIO.lines

  def writeInt(file: String, int: Int): ZIO[Blocking, IOException | InvalidPathException, Unit] =
    FileIO.path(file) >>= (FileIO.writeString(_, int.toString))
