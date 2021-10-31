package aoc

import ops._
import reflect.ClassTag

object imports:
  export ops.Challenge
  export ChallengeOps.{given, *}
  export ConsoleOps.{given, *}
  export NumericOps.{*, given}
  export FileIO.{*, given}
  export StringIO.{*, given}

import imports.*

def none[A] = (_: A) => None

def emptyResult = IllegalArgumentException("No result found")

def splitCsv(s: String) = s.split(',').toList

extension [A,B](a: A) inline def cond(cond: => A => Boolean)(ifTrue: => A => B) = if cond(a) then Some(ifTrue(a)) else None
extension [A](a: A) inline def  adjust(cond: => A => Boolean)(ifTrue: => A => A) = if cond(a) then ifTrue(a) else a

extension (a: Int) inline def th = a-1

extension [A, B, C, D](pair: (A, B)) inline def bimap (f: => A => C, g: => B => D): (C, D) = (f(pair._1), g(pair._2))

final case class Coord(x: Int, y: Int)
final case class CoordL(x: Long, y: Long)

extension (arr: IArray[Long]) def updated(idx: Int, a: Long): IArray[Long] =
  val arr1 = new Array[Long](arr.length)
  System.arraycopy(arr, 0, arr1, 0, arr.length)
  arr1(idx) = a
  arr1.asInstanceOf[IArray[Long]]
