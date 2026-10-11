package controllers

import models.{Cinema, City, MovieRecord, Showtime, SourceData, UsRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel
import tools.costs.PerformanceBudgets

import java.time.LocalDateTime

/**
 * The schedule cache's budget where it broke: a city with a venue on another clock than its own.
 * Green River Cinema keeps Eastern time in Campbellsville, KY, on a page on Central; at 20:15 Central a
 * show at 20:30 on the page's other venues is to come, but the venue's later cut has passed it. Judged
 * against one cut for the whole city, every such film's schedule was rebuilt on every render for up to
 * an hour — 1 MB and 933 µs a render on 40 films (336415df6, when the venue off its city's clock was
 * Russell Springs, an hour BEHIND Eastern Somerset). The fixture corpus's cities hold the same budget
 * in `RenderBudgetSpec`.
 */
class ScheduleCacheBudgetSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val ahead    = UsRoster.byDisplayName("Green River Cinema Campbellsville")
  private val city     = City.forCinema(ahead).value
  private val onTime   = Cinema.all.distinct.find(c => c != ahead && City.forCinema(c).contains(city) &&
                           !UsRoster.venueZones.contains(c)).value
  private val cityTime = LocalDateTime.parse("2026-06-10T20:15")   // CDT; 21:15 EDT at the venue ahead
  private val clock    = java.time.Clock.fixed(cityTime.atZone(city.zoneId).toInstant, java.time.ZoneOffset.UTC)

  /** 40 films at each venue, each with a show that has begun on the city's clock but not on the venue's. */
  private val records = for {
    venue <- Seq(ahead, onTime)
    n     <- 1 to 40
    title  = s"${venue.displayName} film $n"
  } yield (title, Some(2026), MovieRecord(data = Map(venue -> SourceData(title = Some(title), releaseYear = Some(2026),
    showtimes = Seq(20, 22).map(hour => Showtime(LocalDateTime.parse(f"2026-06-10T$hour%02d:30"), None)) ++
      (1 to 20).map(day => Showtime(LocalDateTime.parse("2026-06-10T19:00").plusDays(day.toLong), None))))))

  "a repeated render of a city with a venue off its clock" should "rebuild none of its schedules" in {
    val service = new MovieControllerService(TestReadModel.fromRecords(records), clock)
    val first   = service.toSchedules(city, cityTime)
    first.flatMap(_.showings).flatMap(_._2).map(_.cinema).distinct.toSet shouldBe Set(ahead, onTime)
    val again   = service.toSchedules(city, cityTime)
    PerformanceBudgets.ScheduleRebuildsOnRepeatRender.check(ScheduleRebuilds.between(first, again), s"of ${again.size} schedules")
  }
}
