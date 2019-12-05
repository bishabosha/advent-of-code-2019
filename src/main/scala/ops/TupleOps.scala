package ops

object TupleOps with

  given PairOps: [A,B](pair: (A, B))
    def bimap[C, D](f: A => C, g: B => D): (C, D) = f(pair._1) -> g(pair._2)

end TupleOps
