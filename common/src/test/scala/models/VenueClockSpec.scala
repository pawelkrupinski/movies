package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.*
import tools.MutableClock

import java.time.{Duration, Instant, LocalDate, LocalDateTime, ZoneId}

/** The one source of local dates and times, around the two fall-backs this autumn brings:
 *  Europe on Sunday 25 October 2026 (01:00 UTC), the US on Sunday 1 November 2026 (02:00 local).
 *  Each row is an instant just either side of a local midnight or inside a repeated hour, where a
 *  fixed offset, the wrong zone or the pod's UTC reads a different day or hour. */
class VenueClockSpec extends AnyFlatSpec with Matchers {

  private val dstRows = Table(
    ("zone",                     "instant",              "local time"),
    // Europe, 25 October.
    (TimeZones.Poland,           "2026-10-24T22:30:00Z", "2026-10-25T00:30"), // CEST, +2
    (TimeZones.Poland,           "2026-10-25T00:30:00Z", "2026-10-25T02:30"), // CEST: first 02:30
    (TimeZones.Poland,           "2026-10-25T01:30:00Z", "2026-10-25T02:30"), // CET: the repeat
    (TimeZones.Poland,           "2026-10-25T22:30:00Z", "2026-10-25T23:30"), // CET: +2 would say the 26th
    (TimeZones.UnitedKingdom,    "2026-10-25T23:30:00Z", "2026-10-25T23:30"), // GMT
    (TimeZones.Germany,          "2026-10-25T22:59:00Z", "2026-10-25T23:59"), // CET
    (TimeZones.Spain,            "2026-10-25T23:30:00Z", "2026-10-26T00:30"), // CET: the peninsula is past midnight…
    (TimeZones.Canary,           "2026-10-25T23:30:00Z", "2026-10-25T23:30"), // …the Canaries are not (WET)
    (TimeZones.Canary,           "2026-10-24T23:30:00Z", "2026-10-25T00:30"), // WEST, the night before
    // US, 1 November.
    (TimeZones.UsEastern,        "2026-11-01T05:30:00Z", "2026-11-01T01:30"), // EDT: first 01:30
    (TimeZones.UsEastern,        "2026-11-01T06:30:00Z", "2026-11-01T01:30"), // EST: the repeat
    (TimeZones.UsEastern,        "2026-11-02T04:30:00Z", "2026-11-01T23:30"), // EST: EDT would say the 2nd
    (TimeZones.UsCentral,        "2026-11-02T05:30:00Z", "2026-11-01T23:30"), // CST
    (ZoneId.of("America/Denver"),      "2026-11-02T06:30:00Z", "2026-11-01T23:30"), // MST
    (ZoneId.of("America/Phoenix"),     "2026-11-02T06:30:00Z", "2026-11-01T23:30"), // MST, no DST either side
    (ZoneId.of("America/Los_Angeles"), "2026-11-02T07:30:00Z", "2026-11-01T23:30"), // PST
    (ZoneId.of("America/Los_Angeles"), "2026-11-01T06:30:00Z", "2026-10-31T23:30"), // PDT, the night before
    (ZoneId.of("Pacific/Honolulu"),    "2026-11-02T09:30:00Z", "2026-11-01T23:30"), // HST
  )

  "VenueClock" should "read each zone's own day and hour across the European and US fall-backs" in {
    forAll(dstRows) { (zone, instant, local) =>
      val clock    = new VenueClock(new MutableClock(Instant.parse(instant)))
      val expected = LocalDateTime.parse(local)
      withClue(s"$zone at $instant: ") {
        clock.now(zone) shouldBe expected
        clock.today(zone) shouldBe expected.toLocalDate
      }
    }
  }

  it should "read a venue's day in its own city's zone, not its country's or the fallback's" in {
    val losAngeles = City.all.find(_.zoneId == ZoneId.of("America/Los_Angeles")).getOrElse(fail("no Los Angeles-zone city"))
    val venue      = losAngeles.cinemas.head
    // 05:30 UTC on 2 November: the 2nd in New York (00:30 EST), still the 1st in Los Angeles (21:30 PST).
    val clock = new VenueClock(new MutableClock(Instant.parse("2026-11-02T05:30:00Z")))
    clock.todayAt(venue, fallback = TimeZones.UsEastern) shouldBe LocalDate.parse("2026-11-01")
    clock.nowAt(venue, fallback = TimeZones.UsEastern) shouldBe LocalDateTime.parse("2026-11-01T21:30")
    clock.todayIn(losAngeles) shouldBe LocalDate.parse("2026-11-01")
    clock.today(TimeZones.UsEastern) shouldBe LocalDate.parse("2026-11-02")
  }

  it should "read a venue across a zone line from its metro in the venue's own zone" in {
    // 04:30 UTC on 5 October: 00:30 EDT in Somerset, KY — the metro's majority clock — but 23:30 CDT
    // on the 4th at Russell Springs and at Marianna (Tallahassee's metro, Eastern), and 22:30 MDT
    // at Goodland (Northwest Kansas, Central). On the metro's day each venue's scrape started at
    // the 5th and dropped its still-running evening of the 4th.
    val clock = new VenueClock(new MutableClock(Instant.parse("2026-10-05T04:30:00Z")))
    val rows = Table(
      ("venue",                    "city zone",                      "local now"),
      ("Key Twin Russell Springs", TimeZones.UsEastern,              "2026-10-04T23:30"),
      ("Marianna Cinemas",         TimeZones.UsEastern,              "2026-10-04T23:30"),
      ("Sherman Theatre Goodland", TimeZones.UsCentral,              "2026-10-04T22:30"),
    )
    forAll(rows) { (name, cityZone, local) =>
      val venue = UsRoster.byDisplayName(name)
      City.forCinema(venue).map(_.zoneId) shouldBe Some(cityZone)
      clock.nowAt(venue, fallback = cityZone) shouldBe LocalDateTime.parse(local)
      clock.todayAt(venue, fallback = cityZone) shouldBe LocalDate.parse("2026-10-04")
    }
    UsRoster.venueZones.size shouldBe 30
  }

  it should "answer each ask from the clock, never a date fixed when it was built" in {
    val moving = new MutableClock(Instant.parse("2026-10-24T21:30:00Z"))
    val clock  = new VenueClock(moving)
    clock.todayInPoland shouldBe LocalDate.parse("2026-10-24")
    moving.advance(Duration.ofHours(2))
    clock.todayInPoland shouldBe LocalDate.parse("2026-10-25")
  }

  "VenueClock.fixedOn" should "read the same day in every zone a venue keeps" in {
    val day   = LocalDate.parse("2026-11-01")
    val clock = VenueClock.fixedOn(day)
    dstRows.map(_._1).distinct.foreach(zone => withClue(zone)(clock.today(zone) shouldBe day))
  }
}
