package ops

import zio.*

type Challenge = Has[Challenge.Service]

object Challenge:

  trait Service:
    val sourceFile: List[String]

  object Service:

    def live(files: List[String]): Service = new:
      val sourceFile: List[String] = files

  def makeLayer(files: List[String]): ULayer[Challenge] =
    ZLayer.succeed(Service.live(files))
