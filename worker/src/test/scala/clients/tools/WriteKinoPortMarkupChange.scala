package clients.tools

import models.KinoPort
import tools.RealHttpFetch
import services.cinemas.pl.KinoPortClient

import java.time.LocalDate

/** Record the live gcsw.pl WP-REST repertoire response as it looked on
 *  2026-09-08, after the site restructured its post markup (month header
 *  h3->h4, day headers h4->p) and stopped parsing entirely, for replay by
 *  KinoPortMarkupChangeSpec. See that spec for the root cause. */
object WriteKinoPortMarkupChange {
  def main(args: Array[String]): Unit = {
    val client = new KinoPortClient(
      new RecordingHttpFetch("kinoport-markup-change-2026-09", new RealHttpFetch()),
      KinoPort, LocalDate.of(2026, 9, 8))
    val movies = client.fetch()
    movies.foreach(println)
    println(s"\n=== ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded ===")
  }
}
