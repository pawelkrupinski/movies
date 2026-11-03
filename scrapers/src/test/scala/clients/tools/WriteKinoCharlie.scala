package clients.tools

import models.KinoCharlie
import services.cinemas.pl.MsiClient
import tools.RealHttpFetch

import java.time.LocalDate

/** Record Kino Charlie's MSI portal (bilety.charlie.pl, shared with Kino Spójnia) under
 *  test/resources/fixtures/kino-charlie-msi/, the capture MsiClientSpec replays to split the
 *  two venues by the labels feed's location. Run:
 *    sbt 'scrapers/Test/runMain clients.tools.WriteKinoCharlie <yyyy-mm-dd>'
 *  with the capture date, which the spec then pins as `today`. */
object WriteKinoCharlie {
  def main(args: Array[String]): Unit = {
    val today  = LocalDate.parse(args.head)
    val movies = new MsiClient(new RecordingHttpFetch("kino-charlie-msi", new RealHttpFetch()),
      "https://bilety.charlie.pl", KinoCharlie, today, otherVenueLocations = Set("Kino Spójna")).fetch()
    movies.foreach(m => println(s"${m.movie.title}\t${m.showtimes.size}\t${m.showtimes.map(_.dateTime.toLocalDate).distinct.sorted.mkString(",")}"))
  }
}
