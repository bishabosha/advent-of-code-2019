package aoc

import IntOps._

object IntCodes with
  import Suspend._

  final case class State(mem: IArray[Int], ptr: Int, in: List[Int], out: List[Int])
  enum Suspend with
    case Terminate(state: State)
    case Yield(state: State)
    case Blocked(state: State)
  type Modes = (Arg, Arg, Arg)
  type Arg = Int => (given State) => Int
  type Comp = (given State) => Suspend | State
  type Op = (given Modes) => Comp

  val getTape =
    (inputLine map splitCsv andThen inputInts)
      .filterOrDieMessage(_.nonEmpty)("Expected non empty tape")
      .map(IArray(_:_*))

  def toCode(i: Int) =
    validate.tupled(splitDigits(i, padLeft=5) splitAt 3 bimap(identity, collapse))

  def validate(modes: Array[Int], code: Int): Option[Comp] =
    for op <- Codes.get(code); args <- Some(modes.flatMap(Modes.get))
    if args.length == 3 yield op(given (args(2), args(1), args(0)))

  val Codes = Map[Int, Op](
    1  -> binop(_+_),
    2  -> binop(_*_),
    3  -> inop,
    4  -> outop,
    5  -> jumpop(_!=0),
    6  -> jumpop(_==0),
    7  -> relop(_<_),
    8  -> relop(_==_),
    99 -> nullOp,
  )

  val Modes = Map[Int, Arg](
    0 -> access,
    1 -> value
  )

  def access(offset: Int)(given State) = mem(mem(ptr+offset))
  def value(offset: Int)(given State) = mem(ptr+offset)

  def binop(binOp: (Int, Int) => Int): Op =
    state.copy(mem=mem.updated(mem(ptr+3), binOp(_1,_2)), ptr=ptr+4)
  def relop(cond: (Int, Int) => Boolean): Op =
    binop(cond(_,_) compare false)
  def jumpop(cond: Int => Boolean): Op =
    state.copy(ptr=if cond(_1) then _2 else ptr+3)
  def inop: Op = in match
    case i::is => state.copy(mem=mem.updated(mem(ptr+1),i), ptr=ptr+2, in=is)
    case Nil   => Blocked(state)
  def outop: Op =
    Yield(state.copy(ptr=ptr+2, out=_1::out))
  def nullOp: Op =
    Terminate(state)

  def initial(init: IArray[Int], in: List[Int]) = State(init, 0, in, Nil)

  def concurrent(given State): Either[IllegalStateException, Suspend] =
    toCode(mem(ptr)) match
    case Some(op) => op match
      case given _: State   => concurrent
      case suspend: Suspend => Right(suspend)
    case None => Left(illegalCode)
  end concurrent

  def nonconcurrent(given State): Either[IllegalStateException, State] =
    toCode(mem(ptr)) match
    case Some(op) => op match
      case s: State     => nonconcurrent(given s)
      case n: Yield     => nonconcurrent(given n.state)
      case t: Terminate => Right(t.state)
      case _: Blocked   => Left(illegalConcurrent)
    case None => Left(illegalCode)
  end nonconcurrent

  def illegalConcurrent(given State) =
    IllegalStateException(s"cannot block with op ${mem(ptr)} at Addr($ptr), this fiber is nonconcurrent.")

  def illegalCode(given State) =
    IllegalStateException(s"${mem(ptr)} at Addr($ptr) is not a legal intcode")

  inline def _1(given modes: Modes, state: State) = modes _1 1
  inline def _2(given modes: Modes, state: State) = modes _2 2
  inline def state(given state: State): state.type = state
  inline def mem(given State): state.mem.type = state.mem
  inline def ptr(given State): state.ptr.type = state.ptr
  inline def in(given State): state.in.type = state.in
  inline def out(given State): state.out.type = state.out

end IntCodes
