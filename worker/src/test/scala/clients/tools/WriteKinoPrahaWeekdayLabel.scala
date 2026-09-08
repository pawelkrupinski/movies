package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.PrahaClient

/** Record the live mteatr.pl/pl/repertuar-kino-praha response as it looked on
 *  2026-09-08, after the site started inserting a weekday abbreviation
 *  ("09 Wrz 2026 (Śr) / 16:00") between the year and the slash in its date
 *  stamp, for replay by PrahaWeekdayLabelSpec. See that spec for the root
 *  cause. */
object WriteKinoPrahaWeekdayLabel {
  def main(args: Array[String]): Unit = {
    val client = new PrahaClient(new RecordingHttpFetch("kino-praha-weekday-label-2026-09", new RealHttpFetch()))
    val movies = client.fetch()
    movies.foreach(println)
    println(s"\n=== ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded ===")
  }
}
