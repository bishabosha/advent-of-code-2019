package ops

import zio._

object ConsoleOps:

  given ZioSyntax: {} with

    extension [A](zio: UIO[A]) def sync(): A =
      (new BootstrapRuntime {}).unsafeRun(zio)
