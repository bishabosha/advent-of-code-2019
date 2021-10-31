package aoc

import imports.*

import zio.*
import IntOps.*

import spire.implicits.given

object Day4:

  def digitss(range: Range) =
    range map splitDigits

  def groups(seq: Array[Int], limit: Int) =
    (seq sliding limit map (_.distinct) filter (_.size == 1) flatten) toSet

  def criterion(seq: Array[Int]) =
    (seq sliding 2 exists (_.distinct.size == 1))
    && (seq sliding 2 forall (s => s.head <= s.last))

  def criterionRestrictive(seq: Array[Int]) =
    criterion(seq)
    && (groups(seq, 2) `diff` groups(seq, 3)).nonEmpty

  val Range = raw"(\d{6})-(\d{6})".r

  val getRange = sourceHead >>= ({
    case Range(lo, hi) => UIO.succeed(lo.toInt `to` hi.toInt)
    case fail          => IO.fail(IllegalArgumentException(s"Illegal input $fail"))
  })

  val numberOfPasswords     = getRange `map` digitss `map` (_.filter(criterion).size)
  val numberOfRealPasswords = getRange `map` digitss `map` (_.filter(criterionRestrictive).size)

  val day4_1 = challenge("day4")(numberOfPasswords)
  val day4_2 = challenge("day4")(numberOfRealPasswords)

end Day4
