package ops

import zio._

object ConsoleOps with

  def [A](zio: UIO[A]) sync(): A =
    (new DefaultRuntime {}).unsafeRun(zio)
