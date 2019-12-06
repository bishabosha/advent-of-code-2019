package aoc

import zio._
import Option.when
import IntOps._

import annotation.tailrec

object IntCodes with

  type Result = State | Terminate.type
  final case class State(mem: IArray[Int], ptr: Int, in: List[Int], out: List[Int])
  case object Terminate
  final case class Modes(_1: Int, _2: Int, _3: Int)
  type Comp = (given State) => Result
  type Op = (given Modes) => Comp

  val getTape =
    (inputLine map splitCsv andThen inputInts)
      .filterOrDieMessage(_.nonEmpty)("Expected non empty tape")
      .map(IArray(_:_*))

  def toCode(i: Int) =
    validate.tupled(splitDigits(i, padLeft=5) splitAt 3 bimap(identity, collapse))

  def validate(modes: Array[Int], code: Int): Option[Comp] =
    Codes.get(code).flatMap(op => when(modes.forall(Mode.contains))(op(given Modes(modes(2), modes(1), modes(0)))))

  val Codes = Map[Int, Op](
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

  val Mode = Map(
    0 -> access _,
    1 -> value _
  )

  def access(addr: Int, mem: IArray[Int]) = mem(mem(addr))
  def value(addr: Int, mem: IArray[Int]) = mem(addr)

  inline def arg(pi: => Modes => Int, offset: => Int => Int)(given Modes, State) =
    Mode(pi(modes))(offset(ptr), mem)

  inline def _1(given Modes, State) = arg(_._1,_+1)
  inline def _2(given Modes, State) = arg(_._2,_+2)

  def binop(binOp: (Int, Int) => Int): Op =
    state.copy(mem=mem.updated(mem(ptr+3), binOp(_1,_2)), ptr=ptr+4)
  def relop(cond: (Int, Int) => Boolean): Op =
    binop(cond(_,_) compare false)
  def jumpop(cond: Int => Boolean): Op =
    state.copy(ptr=if cond(_1) then _2 else ptr+3)
  def inop: Op =
    state.copy(mem=mem.updated(mem(ptr+1),in.head), ptr=ptr+2, in=in.tail)
  def outop: Op =
    state.copy(ptr=ptr+2, out=_1::out)
  def nullOp: Op =
    Terminate

  def initial(init: IArray[Int], in: List[Int]) = State(init, 0, in, Nil)

  @tailrec def exec(given State): Either[IllegalStateException, State] =
    toCode(mem(ptr)) match
    case Some(op) =>
      op match
      case given _: State => exec
      case Terminate      => Right(state)
    case None => Left(IllegalStateException(s"${state.mem(state.ptr)} at Addr(${state.ptr}) is not a legal intcode"))
  end exec

  inline def modes(given modes: Modes): modes.type = modes
  inline def state(given state: State): state.type = state
  inline def mem(given state: State): state.mem.type = state.mem
  inline def ptr(given state: State): state.ptr.type = state.ptr
  inline def in(given state: State): state.in.type = state.in
  inline def out(given state: State): state.out.type = state.out

end IntCodes
