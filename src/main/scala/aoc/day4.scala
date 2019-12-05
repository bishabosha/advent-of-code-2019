package aoc

import zio._
import PartialFunction.condOpt

object Day4 with

  def digitss(range: Range) =
    range map IntOps.splitDigits

  def groups(seq: Array[Int], limit: Int) =
    (seq sliding limit map (_.distinct) filter (_.size == 1) flatten) toSet

  def criterion(seq: Array[Int]) =
    (seq sliding 2 exists (_.distinct.size == 1))
    && (seq sliding 2 forall (s => s.head <= s.last))

  def criterionRestrictive(seq: Array[Int]) =
    criterion(seq)
    && (groups(seq, 2) `diff` groups(seq, 3)).nonEmpty

  val Range = raw"(\d{6})-(\d{6})".r

  val getRange =
    for
      line  <- inputLine
      parts =  condOpt(line) { case Range(lo, hi) => lo.toInt to hi.toInt }
      range <- ZIO fromOption parts mapError (_ => IllegalArgumentException(s"illegal input: $line"))
    yield range

  val numberOfPasswords     = getRange map digitss map (_.filter(criterion).size)
  val numberOfRealPasswords = getRange map digitss map (_.filter(criterionRestrictive).size)

  val day4_1 = intChallenge("day4")(numberOfPasswords)
  val day4_2 = intChallenge("day4")(numberOfRealPasswords)

end Day4
