package aoc

import aoc.exports.*
import zio.*

import scala.language.implicitConversions

import IntCodes.*
import Suspend.*

object Day7:

  def outputs(mem: IArray[Long], firstIn: Int, phases: IndexedSeq[Int]) =
    IO.foreachPar(phases.permutations.toArray)(single(mem, firstIn, _))

  def outputsFeedback(mem: IArray[Long], firstIn: Int, phases: IndexedSeq[Int]) =
    IO.foreachPar(phases.permutations.toArray)(phases => runUntilTerminate(firstIn, parallelInit(mem, phases)))

  def single(mem: IArray[Long], firstIn: Int, phases: IndexedSeq[Int]) =
    IO.foldLeft(phases)(firstIn.toLong)((in, phase) =>
      val run = nonconcurrent(initial(mem, phase, in.toInt))
      val res = run.flatMap(_.out.headOption.toRight(IllegalStateException("no next codes")))
      IO.fromEither(res)
    )

  def parallelInit(mem: IArray[Long], phases: Seq[Int]) =
    phases.map(phase => initial(mem, phase)).toList

  def roundRobin(in: Long, states: List[State]) =
    IO.foldLeft(states)((in.toLong, List.empty[State]))((acc, state) =>
      IO.fromEither(concurrent(state.copy(in = state.in #::: LazyList.continually(acc._1)))).map {
        case y: Yield     => (y.state.out.head, y.state.copy(in = LazyList.empty, out = Nil) :: acc._2)
        case _: Terminate => acc
      }
    ).map(_.bimap(x => x, _.reverse))

  def runUntilTerminate(in: Long, states: List[State]): IO[IllegalStateException, Long] =
    for
      pair <- roundRobin(in, states)
      res <- if pair._2.isEmpty then UIO.succeed(pair._1) else IO.effectSuspendTotal(runUntilTerminate.tupled(pair))
    yield res

  def highestSignal(firstIn: Int, phases: IndexedSeq[Int]) =
    (getTape flatMap (outputs(_, firstIn, phases))) map (_.max)

  def highestSignalFeedback(firstIn: Int, phases: IndexedSeq[Int]) =
    (getTape flatMap (outputsFeedback(_, firstIn, phases))) map (_.max)

  val day7_1 = challenge("day7")(highestSignal(0, 0 to 4))
  val day7_2 = challenge("day7")(highestSignalFeedback(0, 5 to 9))

end Day7
