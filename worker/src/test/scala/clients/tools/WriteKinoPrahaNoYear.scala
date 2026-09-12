package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.PrahaClient

/** Record the live mteatr.pl/pl/repertuar-kino-praha response as it looked on
 *  2026-09-12, after the site dropped the 4-digit year from its date stamp
 *  entirely — "12 Wrz (Sb) / 16:10" instead of "09 Wrz 2026 (Śr) / 16:00" —
 *  for replay by PrahaNoYearSpec. See that spec for the root cause. */
object WriteKinoPrahaNoYear {
  def main(args: Array[String]): Unit = {
    val client = new PrahaClient(new RecordingHttpFetch("kino-praha-no-year-2026-09", new RealHttpFetch()))
    val movies = client.fetch()
    movies.foreach(println)
    println(s"\n=== ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded ===")
  }
}
