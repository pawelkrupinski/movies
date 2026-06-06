package clients.tools

import services.cinemas.KinoKulturaClient
import tools.RealHttpFetch

object WriteKinoKultura {
  def main(args: Array[String]): Unit = {
    val client = new KinoKulturaClient(new RecordingHttpFetch("kino-kultura", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
