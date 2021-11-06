package aoc

import ops.{ChallengeOps, ConsoleOps, FileIO, NumericOps, StringIO}

import scala.reflect.ClassTag

object exports:
  export ops.Challenge
  export ChallengeOps.{given, *}
  export ConsoleOps.{given, *}
  export NumericOps.{*, given}
  export FileIO.{*, given}
  export StringIO.{*, given}

def none[A] = (_: A) => None

def emptyResult[A](a: A) = IllegalArgumentException("No result found")

def splitCsv(s: String) = s.split(',').toList

extension [A, B](a: A)
  def cond(cond: A => Boolean)(ifTrue: A => B) =
    if cond(a) then Some(ifTrue(a)) else None

extension [A](a: A)
  def ensureWhen(cond: A => Boolean)(post: A => A) =
    if cond(a) then post(a) else a

extension (a: Int) def th = a - 1

extension [A, B](pair: (A, B))
  def bimap[C, D](f: A => C, g: B => D): (C, D) =
    (f(pair(0)), g(pair(1)))

final case class Coord(x: Int, y: Int)
final case class CoordL(x: Long, y: Long)
