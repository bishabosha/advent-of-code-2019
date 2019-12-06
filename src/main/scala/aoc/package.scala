package aoc

import _root_.ops._
import reflect.ClassTag

export ChallengeOps._, ConsoleOps._, NumericOps.{_, given}, FileIO._, StringIO._, TupleOps.{_,given}

def none[A] = (_: A) => None

def emptyResult = IllegalArgumentException("No result found")

def splitCsv(s: String) = s.split(',').toList

def (arr: IArray[Int]) updated(idx: Int, a: Int): IArray[Int] =
  val arr1 = new Array[Int](arr.length)
  System.arraycopy(arr, 0, arr1, 0, arr.length)
  arr1(idx) = a
  arr1.asInstanceOf[IArray[Int]]
