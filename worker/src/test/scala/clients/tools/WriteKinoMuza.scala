package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinoMuzaClient

object WriteKinoMuza {
  def main(args: Array[String]): Unit = {
    val client = new KinoMuzaClient(new RecordingHttpFetch("kino-muza", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
