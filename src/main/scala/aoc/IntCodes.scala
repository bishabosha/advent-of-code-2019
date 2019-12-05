package aoc

import zio._

object IntCodes with

  type State = (List[Int], Int, List[Int], List[Int], List[Int])
  type Op    = ((Int, Int, Int), Int)

  val getTape = inputLine map (_.split(',').toList) andThen inputInts

  def toCode(i: Int) =
    IntOps.splitDigits(i, padLeft=5) splitAt 3 bimap(a => (a(2), a(1), a(0)), IntOps.collapse)

  val codes = Map(
    1  -> threeAddr(_ + _),
    2  -> threeAddr(_ * _),
    // 3  -> inop,
    // 4  -> outop,
    99 -> nullOp,
  )

  def initialise(noun: Int, verb: Int) =
    ZIO.accessM((tape: List[Int]) =>
      ZIO.effect(tape.updated(1, noun).updated(2, verb)))

  def nullOp(state: State): Option[State] = None

  def threeAddr(binOp: (Int, Int) => Int)(state: State): Option[State] =
    val (tape, ptr, stack, in, out) = state
    val l :: r :: d :: _ = stack
    val tape1 = tape.updated(d, binOp(tape(l), tape(r))) // sequential access
    val ptr1 = ptr + 4
    val _ -> stack1 = tape1.splitAt(ptr1)
    Some(tape1, ptr1, stack1, in, out)
  end threeAddr

  def inop(state: State): Option[State] =
    val (tape, ptr, stack, in, out) = state
    val d :: _ = stack
    val i :: in1 = in
    val tape1 = tape.updated(d, i)
    val ptr1 = ptr + 2
    val _ -> stack1 = tape1.splitAt(ptr1)
    Some(tape1, ptr1, stack1, in1, out)

  def outop(state: State): Option[State] =
    val (tape, ptr, stack, in, out) = state
    val s :: _ = stack
    val ptr1 = ptr + 2
    Some(tape, ptr1, stack, in, tape(s) :: out)

  def exec(ls: List[Int], in: List[Int], out: List[Int]): Either[IllegalStateException, (List[Int], List[Int], List[Int])] =
    @annotation.tailrec
    def inner(tape: List[Int], ptr: Int, stack: List[Int], in: List[Int], out: List[Int]): Either[IllegalStateException, (List[Int], List[Int], List[Int])] =
      stack match
      case Nil => Left(IllegalStateException("No instructions"))
      case i :: stack =>
        codes.get(i) match
          case Some(op) =>
            op(tape, ptr, stack, in, out) match
              case None                            => Right(tape, in, out)
              case Some(tape, ptr, stack, in, out) => inner(tape, ptr, stack, in, out)
          case None => Left(IllegalStateException(s"$i is not an intcode"))
    end inner
    inner(ls, 0, ls, in, out)
  end exec

end IntCodes
