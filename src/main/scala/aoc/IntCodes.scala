package aoc

import IntOps._
import collection.immutable.LongMap

object IntCodes with

  final case class State(mem: IArray[Long], dyn: LongMap[Long], ptr: Int, rel: Long, in: LazyList[Long], out: List[Long])
  enum Suspend with
    case Terminate(state: State)
    case Yield(state: State)
  type Addrs = (Addr, Addr, Addr)
  type Addr = (given State) => Long
  type Fetch = Addr
  type Update = (given State) => IArray[Long] | LongMap[Long]
  type Comp = (given State) => Suspend | State
  type Op = (given Addrs) => Comp

  val getTape =
    (inputLine map splitCsv andThen inputLongs).filterOrDieMessage(_.nonEmpty)("Empty tape").map(IArray(_:_*))

  def step(given State) =
    validate.tupled(splitDigits(mem(ptr), padLeft=5) splitAt 3 bimap(x => x, collapse))

  def validate(mods: Array[Long], op: Long)(given State): Option[Suspend | State] =
    Codes.get(op).flatMap(op => mods.flatMap(Modes.get).cond(_.size==3)(as => op(given (as(2)(1), as(1)(2), as(0)(3)))))

  val Codes = LongMap[Op](
    1L  -> binop(_+_),
    2L  -> binop(_*_),
    3L  -> inop,
    4L  -> outop,
    5L  -> jumpop(_!=0),
    6L  -> jumpop(_==0),
    7L  -> relop(_<_),
    8L  -> relop(_==_),
    9L  -> mvrel,
    99L -> nullOp,
  )

  val Modes = LongMap[Long => Addr](
    0L -> access,
    1L -> value,
    2L -> relative
  )

  def read(addr: Long): Fetch =
    if addr < mem.length then mem(addr.toInt) else dyn.getOrElse(addr, 0)
  def write(addr: Long, value: Long): Update =
    if addr < mem.length then mem.updated(addr.toInt, value) else dyn.updated(addr, value)

  def access(offset: Long): Addr = read(value(offset))
  def value(offset: Long): Addr = ptr+offset
  def relative(offset: Long): Addr = read(value(offset)) + rel

  def binop(binOp: (Long, Long) => Long): Op =
    _3(binOp(_1,_2)) match
    case mem: IArray[Long]  => state.copy(mem=mem, ptr=ptr+4)
    case dyn: LongMap[Long] => state.copy(dyn=dyn, ptr=ptr+4)
  def relop(cond: (Long, Long) => Boolean): Op =
    binop((x,y) => (cond(x,y) compare false).toLong)
  def jumpop(cond: Long => Boolean): Op =
    state.copy(ptr=if cond(_1) then _2.toInt else ptr+3)
  def mvrel: Op =
    state.copy(rel=rel+_1, ptr=ptr+2)
  def inop: Op =
    _1(in.head) match
    case mem: IArray[Long]  => state.copy(mem=mem, in=in.tail, ptr=ptr+2)
    case dyn: LongMap[Long] => state.copy(dyn=dyn, in=in.tail, ptr=ptr+2)
  def outop: Op =
    Suspend.Yield(state.copy(out=_1::out, ptr=ptr+2))
  def nullOp: Op =
    Suspend.Terminate(state)

  def initial(init: IArray[Long], in: List[Int]) = State(init, LongMap.empty, 0, 0L, LazyList(in:_*).map(_.toLong), Nil)

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

  inline def _1(given Addrs): Fetch = read(addrs._1)
  inline def _2(given Addrs): Fetch = read(addrs._2)
  inline def _1(value: Long)(given Addrs): Update = write(addrs._1, value)
  inline def _3(value: Long)(given Addrs): Update = write(addrs._3, value)
  inline def addrs(given addrs: Addrs): addrs.type = addrs
  inline def state(given state: State): state.type = state
  inline def mem(given State): state.mem.type = state.mem
  inline def dyn(given State): state.dyn.type = state.dyn
  inline def rel(given State): state.rel.type = state.rel
  inline def ptr(given State): state.ptr.type = state.ptr
  inline def in(given State): state.in.type = state.in
  inline def out(given State): state.out.type = state.out

end IntCodes
