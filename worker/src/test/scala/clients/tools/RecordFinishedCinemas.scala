package clients.tools

import models._
import services.cinemas._
import tools.RealHttpFetch

import scala.util.Try

/**
 * Records the previously-skipped art-house cinemas that turned out to have a
 * scrapeable source after all: ADA Kino Studyjne (Warszawa, biletyna JSON-LD),
 * Gdyńskie Centrum Filmowe (Gdynia), Kino CSK Lublin (iKSORIS). Run:
 *   sbt 'worker/Test/runMain clients.tools.RecordFinishedCinemas'
 */
object RecordFinishedCinemas {
  def main(args: Array[String]): Unit = {
    val real = new RealHttpFetch()
    def rec(dir: String) = new RecordingHttpFetch(dir, real)
    def report(label: String)(n: => Int): Unit =
      println(f"$label%-26s ${Try(n).fold(e => s"FAIL ${e.getClass.getSimpleName} ${e.getMessage}", x => s"$x films")}")

    report("ADA Kino Studyjne")(new AdaKinoStudyjneClient(rec("ada-kino-studyjne"), AdaKinoStudyjne).fetch().size)
    report("Gdyńskie Centrum Filmowe")(new GdynskieCentrumFilmoweClient(rec("gcf"), GdynskieCentrumFilmowe).fetch().size)
    report("Kino CSK Lublin")(new KinoCskClient(rec("kino-csk"), KinoCskLublin).fetch().size)
  }
}
