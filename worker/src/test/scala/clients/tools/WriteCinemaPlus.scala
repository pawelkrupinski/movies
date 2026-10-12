package clients.tools

import models.{TimeZones, UsRoster, VenueClock}
import services.cinemas.us.CinemaPlusClient
import tools.RealHttpFetch

/** Record Barry Cinemas' Acme Theatre (Riverton, WY) and Gem Theatre (its multi-day
 *  neighbour on the same CinemaPlus site) as fixtures under
 *  test/resources/fixtures/cinemaplus/ for replay by CinemaPlusClientSpec. */
object WriteCinemaPlus {
  def main(args: Array[String]): Unit = {
    val http = new RecordingHttpFetch("cinemaplus", new RealHttpFetch())
    Seq("Acme Theatre Riverton" -> "/acme-theater-riverton", "Gem Theatre Riverton" -> "/gem-theater-riverton").foreach {
      case (name, path) =>
        val cinema = UsRoster.byDisplayName(name)
        val zone   = VenueClock.zoneOf(cinema, TimeZones.UsEastern)
        val movies = new CinemaPlusClient(http, "https://barrycinemas.com", path, cinema,
          today = VenueClock.system.today(zone)).fetch()
        movies.foreach(println)
        println(s"\n=== $name: ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded ===")
    }
  }
}
