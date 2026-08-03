package clients.tools

import models.KinoOskard
import tools.RealHttpFetch
import services.cinemas.pl.Bilety24Client
import services.movies.SingleCountryNormalizer.titleNormalizer

object WriteOskard {
  def main(args: Array[String]): Unit = {
    val oskard = new Bilety24Client(new RecordingHttpFetch("kino-oskard", new RealHttpFetch()), "https://ckis-konin.bilety24.pl", KinoOskard, titles = titleNormalizer)
    println("=== Oskard ===")
    oskard.fetch().foreach(println)
  }
}
