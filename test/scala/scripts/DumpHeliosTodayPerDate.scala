package scripts

import clients.tools.FakeHttpFetch
import services.cinemas.HeliosClient

import java.time.LocalDate

/** One-shot: dump the unique movie titles that the recorded Helios fixture
 *  produces per day, so we can pin `HeliosClientTodayMoviesRegressionSpec`'s
 *  `today` to the date the fixture was captured on.
 *
 *  Run: sbt "Test/runMain scripts.DumpHeliosTodayPerDate"
 */
object DumpHeliosTodayPerDate {
  def main(args: Array[String]): Unit = {
    val client  = new HeliosClient(new FakeHttpFetch("helios/rest-enrichment"))
    val results = client.fetch()
    val allDates = results.flatMap(_.showtimes.map(_.dateTime.toLocalDate)).distinct.sorted

    println(s"Fixture covers ${allDates.size} dates: ${allDates.head} → ${allDates.last}\n")

    allDates.foreach { d: LocalDate =>
      val titles = results
        .filter(_.showtimes.exists(_.dateTime.toLocalDate == d))
        .map(_.movie.title)
        .sorted
      println(s"── $d (${titles.size} titles) ──")
      titles.foreach(t => println(s"  $t"))
      println()
    }
  }
}
