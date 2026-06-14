package clients.tools

import models._
import services.cinemas._
import tools.RealHttpFetch

import scala.util.Try

/**
 * Records the previously-skipped art-house cinemas that turned out to have a
 * scrapeable source after all: ADA Kino Studyjne (Warszawa, biletyna JSON-LD),
 * Gdyńskie Centrum Filmowe (Gdynia). Run:
 *   sbt 'worker/Test/runMain clients.tools.RecordFinishedCinemas'
 */
object RecordFinishedCinemas {
  def main(args: Array[String]): Unit = {
    val real = new RealHttpFetch()
    def rec(directory: String) = new RecordingHttpFetch(directory, real)
    def report(label: String)(n: => Int): Unit =
      println(f"$label%-26s ${Try(n).fold(e => s"FAIL ${e.getClass.getSimpleName} ${e.getMessage}", x => s"$x films")}")

    report("ADA Kino Studyjne")(new BiletynaClient(rec("ada-kino-studyjne"), "https://www.biletyna.pl/Warszawa/ADA-Kino-Studyjne", AdaKinoStudyjne).fetch().size)
    report("Kino Kameralne Cafe")(new BiletynaClient(rec("kino-kameralne"), "https://biletyna.pl/Gdansk/Kino-Kameralne-Cafe", KinoKameralne).fetch().size)
    report("Kino Pegaz (Wodzisław)")(new BiletynaClient(rec("kino-pegaz"), "https://biletyna.pl/Wodzislaw-Slaski/Wodzislawskie-Centrum-Kultury", KinoPegaz).fetch().size)
    report("Gdyńskie Centrum Filmowe")(new GdynskieCentrumFilmoweClient(rec("gcf"), GdynskieCentrumFilmowe).fetch().size)
  }
}
