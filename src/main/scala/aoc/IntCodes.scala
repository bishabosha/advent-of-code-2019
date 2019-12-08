package aoc

import IntOps._

object IntCodes with

  final case class State(mem: IArray[Int], ptr: Int, in: LazyList[Int], out: List[Int])
  enum Suspend with
    case Terminate(state: State)
    case Yield(state: State)
  type Modes = (Arg, Arg, Arg)
  type Arg = Int => (given State) => Int
  type Comp = (given State) => Suspend | State
  type Op = (given Modes) => Comp

  val getTape =
    (inputLine map splitCsv andThen inputInts).filterOrDieMessage(_.nonEmpty)("Empty tape").map(IArray(_:_*))

  def step(given State) =
    validate.tupled(splitDigits(mem(ptr), padLeft=5) splitAt 3 bimap(x => x, collapse))

  def validate(modes: Array[Int], code: Int)(given State): Option[Suspend | State] =
    Codes.get(code).flatMap(op => modes.flatMap(Modes.get).cond(_.length == 3)(as => op(given (as(2), as(1), as(0)))))

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
    Suspend.Yield(state.copy(ptr=ptr+2, out=_1::out))
  def nullOp: Op =
    Suspend.Terminate(state)

  def initial(init: IArray[Int], in: List[Int]) = State(init, 0, LazyList(in:_*), Nil)

  def concurrent(state: State): Either[IllegalStateException, Suspend] =
    step(given state) match
    case Some(state: State)     => concurrent(state)
    case Some(suspend: Suspend) => Right(suspend)
    case None                   => Left(illegalCode(given state))
  end concurrent

  def nonconcurrent(state: State): Either[IllegalStateException, State] =
    step(given state) match
    case Some(res) => res match
      case s: State             => nonconcurrent(s)
      case n: Suspend.Yield     => nonconcurrent(n.state)
      case t: Suspend.Terminate => Right(t.state)
    case None => Left(illegalCode(given state))
  end nonconcurrent

  def illegalCode(given State) = IllegalStateException(s"${mem(ptr)} at Addr($ptr) is not a legal intcode")

  inline def _1(given modes: Modes, state: State) = modes _1 1
  inline def _2(given modes: Modes, state: State) = modes _2 2
  inline def state(given state: State): state.type = state
  inline def mem(given State): state.mem.type = state.mem
  inline def ptr(given State): state.ptr.type = state.ptr
  inline def in(given State): state.in.type = state.in
  inline def out(given State): state.out.type = state.out

end IntCodes
