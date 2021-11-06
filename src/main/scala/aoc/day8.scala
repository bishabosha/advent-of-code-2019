package aoc

import exports.*

import zio._

object Day8:

  def layers(w: Int, h: Int)(src: String) =
    src.cond(_.size % (w*h) == 0)(_ `grouped` (w*h) toIndexedSeq) `toRight` dimErr

  def minZeros(src: Seq[String]) =
    (src groupBy(_ `filter` (_=='0') length) minBy(_._1))._2.cond(_.length == 1)(_.head) `toRight` layerErr

  def onesByTwos(src: String) =
    (src.view `filter` (c => c == '1' || c == '2') `groupBy` identity values) `map` (_.size) product

  def stacked(src: Seq[String]) =
    (src transpose) `map` (_ `dropWhile` (_=='2') head)

  def getLayers(w: Int, h: Int) =
    sourceHead >>= layers(w,h) andThen IO.fromEither

  def filtered(w: Int, h: Int) =
    getLayers(w,h) >>= minZeros andThen (_ `map` onesByTwos) andThen IO.fromEither

  def message(w: Int, h: Int) =
    getLayers(w,h) `map` (stacked andThen (_ `grouped` w `map` (_ `map` toAscii mkString) mkString("\n","\n","")))

  def toAscii(i: Char) = if i == '0' then ' ' else '*'

  def dimErr   = IllegalArgumentException("data does not match dimensions")
  def layerErr = IllegalArgumentException("more than one layer with min zeros")

  val day8_1 = challenge("day8")(filtered(25,6))
  val day8_2 = challenge("day8")(message(25,6))

end Day8
