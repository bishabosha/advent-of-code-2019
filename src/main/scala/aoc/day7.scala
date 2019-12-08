package aoc

import zio._
import IntCodes._
import Suspend._

object Day7

  def outputs(mem: IArray[Int], firstIn: Int, phases: IndexedSeq[Int]) =
    IO.foreachPar(phases.permutations.toArray)(single(mem,firstIn,_))

  def outputsFeedback(mem: IArray[Int], firstIn: Int, phases: IndexedSeq[Int]) =
    IO.foreachPar(phases.permutations.toArray)(phases => runUntilTerminate(firstIn, parallelInit(mem,phases)))

  def single(mem: IArray[Int], firstIn: Int, phases: IndexedSeq[Int]) =
    IO.foldLeft(phases)(firstIn)((in, phase) =>
      for out <- IO.fromEither(nonconcurrent(given initial(mem, phase :: in :: Nil)).map(_.out)) if out.nonEmpty
      yield out.head
    )

  def parallelInit(mem: IArray[Int], phases: Seq[Int]) =
    phases.map(phase => initial(mem, phase::Nil)).toList

  def roundRobin(in: Int, states: List[State]) =
    IO.foldLeft(states)((in, List.empty[State]))((acc, state) => {
      def run(state: State): IO[IllegalStateException, (Int, List[State])] =
        IO.fromEither(concurrent(given state)).flatMap {
          case b: Blocked   => IO.effectSuspendTotal(run(b.state.copy(in=acc._1::Nil)))
          case y: Yield     => UIO.succeed(y.state.out.head, y.state::acc._2)
          case _: Terminate => UIO.succeed(acc)
        }
      run(state)
    }).map(_.bimap(identity, _.reverse))

  def runUntilTerminate(in: Int, states: List[State]): IO[IllegalStateException, Int] = for
    pair <- roundRobin(in, states)
    res  <- if pair._2.isEmpty then UIO.succeed(pair._1) else IO.effectSuspendTotal(runUntilTerminate.tupled(pair))
  yield res

  def highestSignal(firstIn: Int, phases: IndexedSeq[Int]) =
    (getTape >>= (outputs(_, firstIn, phases))) map (_.max)

  def highestSignalFeedback(firstIn: Int, phases: IndexedSeq[Int]) =
    (getTape >>= (outputsFeedback(_, firstIn, phases))) map (_.max)

  val day7_1 = challenge("day7")(highestSignal(0, 0 to 4))
  val day7_2 = challenge("day7")(highestSignalFeedback(0, 5 to 9))

end Day7
