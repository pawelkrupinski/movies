package services.cinemas

import clients.tools.ScriptedByUrlHttpFetch
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{FlicksClient, WebediaShowtimesClient}
import models.VenueClock
import tools.MutableClock

import org.scalatest.prop.TableDrivenPropertyChecks.*

import java.time.{Duration, Instant, ZoneId}

/** Scrapers are built once, at boot, and asked for their day at each scrape — in the venue's
 *  own zone. Before, every client got the catalogue's Warsaw `LocalDate`, fixed at boot. */
class VenueDayPlanningSpec extends AnyFlatSpec with Matchers with OptionValues {

  // A Flicks programme page whose day tabs run 1–4 August.
  private val programme = new ScriptedByUrlHttpFetch(_ =>
    """<div class="timetable timetable--cinema">""" +
      Seq("2026-08-01", "2026-08-02", "2026-08-03", "2026-08-04")
        .map(d => s"""<div class="timetable__day" data-date="$d"></div>""").mkString + "</div>")

  private def usFlicksVenue(clock: MutableClock): FlicksClient =
    new CinemaScraperCatalog(programme, venueClock = new VenueClock(clock))
      .all.find(_.cinema.displayName == "AMC Town Center 20").value.asInstanceOf[FlicksClient]

  "a US venue scraped at 01:00 Warsaw" should "keep that US evening's screenings" in {
    // 23:00 UTC on 1 August: 01:00 on the 2nd in Warsaw, 18:00 on the 1st in Kansas.
    val clock = new MutableClock(Instant.parse("2026-08-01T23:00:00Z"))
    usFlicksVenue(clock).planChunks() should contain ("2026-08-01")
  }

  "a scraper built at boot" should "plan from the day it scrapes on, not the day it was built" in {
    val clock = new MutableClock(Instant.parse("2026-08-01T18:00:00Z"))
    val venue = usFlicksVenue(clock)
    venue.planChunks().headOption.value shouldBe "2026-08-01"
    clock.advance(Duration.ofDays(2))
    venue.planChunks() shouldBe Seq("2026-08-03", "2026-08-04")
  }

  // Days around the US fall-back (Sunday 1 November 2026).
  private val fallBackProgramme = new ScriptedByUrlHttpFetch(_ =>
    """<div class="timetable timetable--cinema">""" +
      Seq("2026-10-31", "2026-11-01", "2026-11-02", "2026-11-03")
        .map(d => s"""<div class="timetable__day" data-date="$d"></div>""").mkString + "</div>")

  /** The first flicks.us venue whose city keeps `zone`. */
  private def flicksVenueIn(zone: String, clock: MutableClock): FlicksClient =
    new CinemaScraperCatalog(fallBackProgramme, venueClock = new VenueClock(clock)).all.collect {
      case f: FlicksClient if f.sourceUrl.exists(_.contains("flicks.us")) &&
        models.City.forCinema(f.cinema).exists(_.zoneId == ZoneId.of(zone)) => f
    }.headOption.getOrElse(fail(s"no flicks.us venue in $zone"))

  // A US venue plans from ITS OWN calendar day, not New York's: between midnight and 03:00
  // Eastern the western zones are still on the previous evening, and dropping that day tab
  // left the evening's screenings out of a listing marked complete.
  private val usZones = Table(
    ("zone",                "instant",              "still on 1 November"),
    ("America/New_York",    "2026-11-02T05:30:00Z", false), // 00:30 EST on the 2nd (control)
    ("America/Chicago",     "2026-11-02T05:30:00Z", true),  // 23:30 CST — CDT would read 00:30
    ("America/Denver",      "2026-11-02T06:30:00Z", true),  // 23:30 MST
    ("America/Phoenix",     "2026-11-02T06:30:00Z", true),  // 23:30 MST, no DST either side
    ("America/Los_Angeles", "2026-11-02T07:30:00Z", true),  // 23:30 PST — PDT would read 00:30
    ("Pacific/Honolulu",    "2026-11-02T09:30:00Z", true),  // 23:30 HST
  )

  "a flicks.us venue" should "plan from its own zone's calendar day across the US fall-back" in {
    forAll(usZones) { (zone, instant, stillOnFirst) =>
      val planned = flicksVenueIn(zone, new MutableClock(Instant.parse(instant))).planChunks()
      withClue(s"$zone at $instant planned $planned: ") {
        planned.contains("2026-11-01") shouldBe stillOnFirst
        planned should contain ("2026-11-02")
      }
    }
  }

  // Spain has two zones: the Canary provinces run an hour behind the peninsula.
  private def sensacineVenueIn(zone: String, clock: MutableClock): WebediaShowtimesClient = {
    val page = new ScriptedByUrlHttpFetch(_ =>
      """<div data-showtimes-dates="[&quot;2026-10-24&quot;,&quot;2026-10-25&quot;,&quot;2026-10-26&quot;]"></div>""")
    new CinemaScraperCatalog(page, venueClock = new VenueClock(clock)).all.collect {
      case w: WebediaShowtimesClient if w.sourceUrl.exists(_.contains("sensacine")) &&
        models.City.forCinema(w.cinema).exists(_.zoneId == ZoneId.of(zone)) => w
    }.headOption.getOrElse(fail(s"no SensaCine venue in $zone"))
  }

  private val spanishZones = Table(
    ("zone",            "instant",              "planned from"),
    ("Europe/Madrid",   "2026-10-24T22:30:00Z", "2026-10-25"), // 00:30 CEST on the 25th (control)
    ("Atlantic/Canary", "2026-10-24T22:30:00Z", "2026-10-24"), // 23:30 WEST on the 24th
    ("Atlantic/Canary", "2026-10-25T23:30:00Z", "2026-10-25"), // 23:30 WET, after the fall-back
  )

  "a SensaCine venue" should "plan from its own province's calendar day, Canary included" in {
    forAll(spanishZones) { (zone, instant, from) =>
      val planned = sensacineVenueIn(zone, new MutableClock(Instant.parse(instant))).planChunks()
      withClue(s"$zone at $instant planned $planned: ") { planned.headOption.value shouldBe from }
    }
  }
}
