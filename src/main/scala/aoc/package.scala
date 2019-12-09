package aoc

import _root_.ops._
import reflect.ClassTag

export ChallengeOps._, ConsoleOps._, NumericOps.{_, given}, FileIO._, StringIO._

def none[A] = (_: A) => None

def emptyResult = IllegalArgumentException("No result found")

def splitCsv(s: String) = s.split(',').toList

inline def [A,B](a: A) cond(cond: => A => Boolean)(ifTrue: => A => B) = if cond(a) then Some(ifTrue(a)) else None

inline def [A, B, C, D](pair: (A, B)) bimap (f: => A => C, g: => B => D): (C, D) = (f(pair._1), g(pair._2))

def [A: ClassTag](arr: IArray[A]) updated(idx: Int, a: A): IArray[A] =
  val arr1 = new Array[A](arr.length)
  System.arraycopy(arr, 0, arr1, 0, arr.length)
  arr1(idx) = a
  arr1.asInstanceOf[IArray[A]]
