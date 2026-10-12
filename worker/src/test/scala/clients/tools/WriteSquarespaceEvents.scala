package clients.tools

import models.{TimeZones, UsRoster, VenueClock}
import services.cinemas.us.{SquarespaceEventsClient, UsIndependentVenues}
import tools.RealHttpFetch

/** Record ACME Screening Room's live Squarespace events collection as a fixture under
 *  test/resources/fixtures/squarespace/ for replay by SquarespaceEventsClientSpec. */
object WriteSquarespaceEvents {
  def main(args: Array[String]): Unit = {
    val name   = "ACME Screening Room Lambertville"
    val venue  = UsIndependentVenues.squarespace(name)
    val zone   = TimeZones.UsEastern
    val http   = new RecordingHttpFetch("squarespace", new RealHttpFetch())
    val movies = new SquarespaceEventsClient(http, venue.siteUrl, venue.eventsPath, UsRoster.byDisplayName(name), zone,
      today = VenueClock.system.today(zone)).fetch()
    movies.foreach(println)
    println(s"\n=== ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded ===")
  }
}
