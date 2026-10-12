package clients.tools

import models.PhoenixCinemaEastFinchley
import tools.RealHttpFetch
import services.cinemas.uk.SavoyClient

/** Record the live Savoy Systems `WhatsOn` page as a fixture under
 *  test/resources/fixtures/savoy/ for replay by SavoyClientSpec. */
object WriteSavoy {
  def main(args: Array[String]): Unit = {
    val http   = new RecordingHttpFetch("savoy", new RealHttpFetch())
    val movies = new SavoyClient(http, "https://www.phoenixcinema.co.uk/PhoenixCinemaLondon.dll/", PhoenixCinemaEastFinchley,
      today = models.VenueClock.system.today(models.TimeZones.UnitedKingdom)).fetch()
    movies.foreach(println)
    println(s"\n=== ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded ===")
  }
}
