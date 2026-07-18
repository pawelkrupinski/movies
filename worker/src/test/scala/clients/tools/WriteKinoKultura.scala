package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinoKulturaClient

object WriteKinoKultura {
  def main(args: Array[String]): Unit = {
    val client = new KinoKulturaClient(new RecordingHttpFetch("kino-kultura", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
