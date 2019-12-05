package aoc

import _root_.ops._

export ChallengeOps._, ConsoleOps._, NumericOps.{_, given}, FileIO._, StringIO._, TupleOps.{_,given}

def none[A] = (_: A) => None

def emptyResult = IllegalArgumentException("No result found")

def splitCsv(s: String) = s.split(',').toList
