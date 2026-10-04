package controllers

import models.{City, MovieRecord, Showtime, SourceData, UsRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel

import java.time.{LocalDateTime, ZoneId}

/**
 * Key Twin Russell Springs keeps Central time inside Somerset, KY — a metro on Eastern, its
 * venues' majority (`UsRoster.venueZones`). The listing judged its showtimes started on the
 * city's clock, an hour ahead of the venue's: a 20:30 show vanished at 19:45 its time, before
 * it began.
 */
class StartedShowtimeCutSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val venue    = UsRoster.byDisplayName("Key Twin Russell Springs")
  private val city     = City.forCinema(venue).value
  private val cityTime = LocalDateTime.parse("2026-06-10T21:15")   // EDT; 20:15 CDT at the venue
  private val clock    = java.time.Clock.fixed(cityTime.atZone(city.zoneId).toInstant, java.time.ZoneOffset.UTC)

  "the listing" should "judge a venue across a zone line from its city on the venue's own clock" in {
    city.zoneId shouldBe ZoneId.of("America/New_York")
    val record = MovieRecord(data = Map(venue -> SourceData(title = Some("Late Show"), releaseYear = Some(2026),
      showtimes = Seq(
        Showtime(LocalDateTime.parse("2026-06-10T19:30"), None),   // began 45 min ago, venue time: lapsed
        Showtime(LocalDateTime.parse("2026-06-10T20:30"), None)))))  // starts in 15 min, venue time
    val readModel = TestReadModel.fromRecords(Seq(("Late Show", Some(2026), record)))
    val service   = new MovieControllerService(readModel, clock)

    val schedule = service.toSchedules(city, cityTime).find(_.movie.title == "Late Show").value
    schedule.showings.flatMap(_._2).flatMap(_.showtimes).map(_.dateTime) shouldBe
      Seq(LocalDateTime.parse("2026-06-10T20:30"))

    // The page's own expiry agrees: the pill lapses 30 min after 20:30 CENTRAL, not Eastern.
    val html = views.html._filmShowings(schedule)(using city).body
    val lapse = LocalDateTime.parse("2026-06-10T21:00").atZone(ZoneId.of("America/Chicago")).toInstant.toEpochMilli
    html should include (s"""data-expires="$lapse"""")
  }
}
