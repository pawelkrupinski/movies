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

  // The schedule cache reused a film's schedule while its earliest showtime was after the city's
  // LATEST venue cut. A venue behind its city's clock keeps a showtime the city's cut has already
  // passed, so its film's earliest stayed under that cut and every render rebuilt it — for up to an
  // hour, every such film of the city.
  it should "reuse a schedule whose earliest showtime is upcoming only on its venue's earlier clock" in {
    val record = MovieRecord(data = Map(venue -> SourceData(title = Some("Late Show"), releaseYear = Some(2026),
      showtimes = Seq(Showtime(LocalDateTime.parse("2026-06-10T20:30"), None)))))  // 20:30 CDT = 21:30 EDT
    val readModel = TestReadModel.fromRecords(Seq(("Late Show", Some(2026), record)))
    val service   = new MovieControllerService(readModel, clock)

    val first  = service.toSchedules(city, cityTime).find(_.movie.title == "Late Show").value
    val second = service.toSchedules(city, cityTime).find(_.movie.title == "Late Show").value
    withClue("nothing changed between the two renders, so the second must reuse the first's schedule: ") {
      second should be theSameInstanceAs first
    }
  }
}
