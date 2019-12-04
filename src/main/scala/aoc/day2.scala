package aoc

import zio._
import PartialFunction.condOpt

object Day2 with

  val getTape = inputLine `map` (_.split(',').toList) andThen inputInts

  def initialise(noun: Int, verb: Int) =
    ZIO.accessM((tape: List[Int]) =>
      ZIO.effect(tape.updated(1, noun).updated(2, verb)))

  def tabulate(range: Range) = range.map(n => range.map(n -> _)).flatten

  def search(goal: Int, size: Range) =
    def findFirst(options: List[Option[(Int, Int)]]) =
      options.collectFirst { case Some(n,v) => f"$n%2d$v%2d" }
    for
      options <- ZIO.foreach(tabulate(size))((n, v) =>
                    prog(n, v).fold(_ => None, condOpt(_)({ case x if x == goal => (n, v) })))
      result  <- ZIO.fromOption(findFirst(options)).mapError(_ => IllegalArgumentException("No result found"))
    yield
      result

  def prog(noun: Int, verb: Int) = for
    tape  <- initialise(noun, verb)
    ints  <- ZIO.fromEither(exec(tape))
    first <- ZIO.effect(ints.head)
  yield first

  val day2_1 = intChallenge("day2")(prog(noun=12, verb=2) `compose` getTape)
  val day2_2 = stringChallenge("day2")(search(19690720, 0 to 99) `compose` getTape)

  val codes = Map(
    1  -> threeAddr(_ + _),
    2  -> threeAddr(_ * _),
    99 -> nullOp,
  )

  type State = (List[Int], Int, List[Int])

  def nullOp(state: State): Option[State] = None

  def threeAddr(binOp: (Int, Int) => Int)(state: State): Option[State] =
    val (tape, ptr, stack) = state
    val l :: r :: d :: _ = stack
    val tape1 = tape.updated(d, binOp(tape(l), tape(r))) // sequential access
    val ptr1 = ptr + 4
    val _ -> stack1 = tape1.splitAt(ptr1)
    Some(tape1, ptr1, stack1)
  end threeAddr

  def exec(ls: List[Int]): Either[IllegalStateException, List[Int]] =
    @annotation.tailrec
    def inner(tape: List[Int], ptr: Int, stack: List[Int]): Either[IllegalStateException, List[Int]] =
      stack match
      case Nil => Left(IllegalStateException("No instructions"))
      case i :: stack =>
        codes.get(i) match
          case Some(op) =>
            op(tape, ptr, stack) match
              case None                   => Right(tape)
              case Some(tape, ptr, stack) => inner(tape, ptr, stack)
          case None => Left(IllegalStateException(s"$i is not an intcode"))
    end inner
    inner(ls, 0, ls)
  end exec

end Day2
