package clients.tools

import models.{KinoGlebocka66, KinoNaBoku}
import tools.RealHttpFetch
import services.cinemas.pl.BokClient
import services.movies.SingleCountryNormalizer.titleNormalizer

object WriteBok {
  def main(args: Array[String]): Unit = {
    val naBoku = new BokClient(new RecordingHttpFetch("kino-na-boku", new RealHttpFetch()), "kino-na-boku", KinoNaBoku, titles = titleNormalizer, today = models.VenueClock.system.todayInPoland)
    println("=== na Boku ===")
    naBoku.fetch().foreach(println)

    val glebocka = new BokClient(new RecordingHttpFetch("kino-glebocka-66", new RealHttpFetch()), "kino-glebocka-66", KinoGlebocka66, titles = titleNormalizer, today = models.VenueClock.system.todayInPoland)
    println("=== Głębocka 66 ===")
    glebocka.fetch().foreach(println)
  }
}
