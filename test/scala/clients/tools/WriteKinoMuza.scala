package clients.tools

import services.cinemas.KinoMuzaClient
import tools.RealHttpFetch

object WriteKinoMuza {
  def main(args: Array[String]): Unit = {
    val client = new KinoMuzaClient(new RecordingHttpFetch("kino-muza", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
