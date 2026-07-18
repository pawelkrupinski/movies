package clients.tools

import models.KinoAtlantic
import tools.RealHttpFetch
import services.cinemas.pl.NoveKinoClient

object WriteNoveKino {
  def main(args: Array[String]): Unit = {
    val client = new NoveKinoClient(new RecordingHttpFetch("kino-atlantic", new RealHttpFetch()), "atlantic", KinoAtlantic)
    client.fetch().foreach(println)
  }
}
