package aoc

import IntOps._

object IntCodes with

  type Result = State | Terminate.type
  final case class State(mem: IArray[Int], ptr: Int, in: List[Int], out: List[Int])
  case object Terminate
  type Modes = (Arg, Arg, Arg)
  type Arg = Int => (given State) => Int
  type Comp = (given State) => Result
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
  def inop: Op =
    state.copy(mem=mem.updated(mem(ptr+1),in.head), ptr=ptr+2, in=in.tail)
  def outop: Op =
    state.copy(ptr=ptr+2, out=_1::out)
  def nullOp: Op =
    Terminate

  def initial(init: IArray[Int], in: List[Int]) = State(init, 0, in, Nil)

  def exec(given State): Either[IllegalStateException, State] =
    toCode(mem(ptr)) match
    case Some(op) => op match
      case given _: State => exec
      case Terminate      => Right(state)
    case None => Left(IllegalStateException(s"${state.mem(state.ptr)} at Addr(${state.ptr}) is not a legal intcode"))
  end exec

  inline def _1(given modes: Modes, state: State) = modes _1 1
  inline def _2(given modes: Modes, state: State) = modes _2 2
  inline def state(given state: State): state.type = state
  inline def mem(given State): state.mem.type = state.mem
  inline def ptr(given State): state.ptr.type = state.ptr
  inline def in(given State): state.in.type = state.in
  inline def out(given State): state.out.type = state.out

end IntCodes
