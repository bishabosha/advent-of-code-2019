package aoc

import zio._
import Option.when
import IntOps._

object IntCodes with

  type State = (List[Int], Int, List[Int], List[Int], List[Int]) // memory, ptr, stack, in, out
  type Modes = (Int, Int, Int)

  val getTape = inputLine map splitCsv andThen inputInts

  def toCode(i: Int) =
    validate.tupled(splitDigits(i, padLeft=5) splitAt 3 bimap(identity, collapse))

  def validate(modes: Array[Int], code: Int) =
    Codes.get(code).flatMap(op => when(modes.forall(Modes.contains))(op(modes(2), modes(1), modes(0))))

  val Codes = Map(
    1  -> binop(_ + _),
    2  -> binop(_ * _),
    3  -> inop,
    4  -> outop,
    5  -> jumpop(_ != 0),
    6  -> jumpop(_ == 0),
    7  -> relop(_ < _),
    8  -> relop(_ == _),
    99 -> nullOp,
  )

  val Modes = Map(
    0 -> access _,
    1 -> value _
  )

  def value(i: Int, mem: List[Int]) = i
  def access(i: Int, mem: List[Int]) = mem(i)

  def nullOp(modes: Modes)(state: State): Option[State] = None

  def binop(binOp: (Int, Int) => Int)(modes: Modes)(state: State): Option[State] =
    val (mem, ptr, stack, in, out) = state
    val (lmode, rmode, _) = modes
    val l :: r :: d :: _ = stack
    val lArg = Modes(lmode)(l, mem)
    val rArg = Modes(rmode)(r, mem)
    val mem1 = mem.updated(d, binOp(lArg, rArg))
    val ptr1 = ptr + 4
    val stack1 = mem1.splitAt(ptr1)._2
    Some(mem1, ptr1, stack1, in, out)
  end binop

  def relop(cond: (Int, Int) => Boolean)(modes: Modes)(state: State): Option[State] =
    binop((l, r) => if cond(l,r) then 1 else 0)(modes)(state)

  def jumpop(cond: Int => Boolean)(modes: Modes)(state: State): Option[State] =
    val (mem, ptr, stack, in, out) = state
    val c :: i :: _ = stack
    val ptr1 =
      if cond(Modes(modes._1)(c, mem)) then Modes(modes._2)(i, mem)
      else ptr + 3
    val stack1 = mem.splitAt(ptr1)._2
    Some(mem, ptr1, stack1, in, out)
  end jumpop

  def inop(modes: Modes)(state: State): Option[State] =
    val (mem, ptr, stack, in, out) = state
    val d :: _ = stack
    val i :: in1 = in.ensuring(_ != Nil)
    val mem1 = mem.updated(d, i)
    val ptr1 = ptr + 2
    val stack1 = mem1.splitAt(ptr1)._2
    Some(mem1, ptr1, stack1, in1, out)
  end inop

  def outop(modes: Modes)(state: State): Option[State] =
    val (mem, ptr, stack, in, out) = state
    val v :: stack1 = stack
    val o = Modes(modes._1)(v, mem)
    Some(mem, ptr + 2, stack1, in, o :: out)
  end outop

  def exec(ls: List[Int], in: List[Int]): Either[IllegalStateException, (List[Int], List[Int], List[Int])] =
    @annotation.tailrec
    def inner(mem: List[Int], ptr: Int, stack: List[Int], in: List[Int], out: List[Int]): Either[IllegalStateException, (List[Int], List[Int], List[Int])] =
      stack match
      case i :: stack =>
        toCode(i) match
        case Some(op) =>
          op(mem, ptr, stack, in, out) match
          case Some(mem, ptr, stack, in, out) => inner(mem, ptr, stack, in, out)
          case None                           => Right(mem, in, out)
        case None => Left(IllegalStateException(s"$i is not a legal intcode"))
      case Nil => Left(IllegalStateException("No instructions"))
    end inner
    inner(ls, 0, ls, in, Nil)
  end exec

end IntCodes
