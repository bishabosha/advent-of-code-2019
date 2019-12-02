package ops

import zio._

object ConsoleOps with

  def [A](zio: URIO[ZEnv, A]) sync(): A =
    (new DefaultRuntime {}).unsafeRun(zio)
