package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinoMuzaClient
import services.movies.SingleCountryNormalizer.titleNormalizer

object WriteKinoMuza {
  def main(args: Array[String]): Unit = {
    val client = new KinoMuzaClient(new RecordingHttpFetch("kino-muza", new RealHttpFetch()), titles = titleNormalizer)
    client.fetch().foreach(println)
  }
}
