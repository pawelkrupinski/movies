package clients.tools

import services.cinemas.KinoMuzaClient
import tools.RealHttpFetch

object WriteKinoMuza extends App {
  private val client = new KinoMuzaClient(new RecordingHttpFetch("kino-muza", new RealHttpFetch()))
  client.fetch().foreach(println)
}
