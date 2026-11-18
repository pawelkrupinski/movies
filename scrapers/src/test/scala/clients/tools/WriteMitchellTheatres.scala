package clients.tools

import models.{TimeZones, UsRoster, VenueClock}
import services.cinemas.us.MitchellTheatresClient
import tools.RealHttpFetch

/** Record Southgate Cinema 6's (Liberal, KS) own site as a fixture under
 *  test/resources/fixtures/mitchelltheatres/ for replay by MitchellTheatresClientSpec. */
object WriteMitchellTheatres {
  def main(args: Array[String]): Unit = {
    val http   = new RecordingHttpFetch("mitchelltheatres", new RealHttpFetch())
    val cinema = UsRoster.byDisplayName("Southgate Cinema 6 Liberal")
    val zone   = VenueClock.zoneOf(cinema, TimeZones.UsEastern)
    val movies = new MitchellTheatresClient(http, "https://www.southgate6.com", cinema,
      today = VenueClock.system.today(zone)).fetch()
    movies.foreach(println)
    println(s"\n=== ${cinema.displayName}: ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded ===")
  }
}
