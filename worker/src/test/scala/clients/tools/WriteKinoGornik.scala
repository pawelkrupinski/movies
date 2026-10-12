package clients.tools

import models.KinoGornikLeczyca
import services.cinemas.pl.MsiClient
import tools.RealHttpFetch

import java.time.LocalDate

/** Record Kino Górnik Łęczyca's MSI month pages under test/resources/fixtures/kino-gornik-leczyca/, the
 *  capture MsiClientSpec replays for the venue's distributor tails. Run:
 *    sbt 'worker/Test/runMain clients.tools.WriteKinoGornik <yyyy-mm-dd>'
 *  with the capture date, which the spec then pins as `today`. */
object WriteKinoGornik {
  def main(args: Array[String]): Unit = {
    val today  = LocalDate.parse(args.head)
    val movies = new MsiClient(new RecordingHttpFetch("kino-gornik-leczyca", new RealHttpFetch()),
      "https://kinogornik.eurobilet.pl", KinoGornikLeczyca, today).fetch()
    movies.foreach(m => println(s"${m.movie.title}\t${m.showtimes.flatMap(_.format).distinct.mkString(",")}"))
  }
}
