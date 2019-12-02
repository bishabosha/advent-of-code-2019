package ops

import zio._

import java.lang.NumberFormatException

object StringIO with

  def parseInt(str: String): IO[NumberFormatException, Int] =
    IO.effect(str.toInt).refineToOrDie
