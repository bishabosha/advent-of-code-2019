package ops

trait Challenge with

  val challenge: Challenge.Service[Any]

object Challenge with

  trait Service[R](val sourceFile: List[String])

  trait Live(sourceFile: List[String]) extends Challenge with

    val challenge: Challenge.Service[Any] = new Service[Any](sourceFile) {}
