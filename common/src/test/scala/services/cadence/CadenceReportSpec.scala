package services.cadence

import org.scalatest.LoneElement._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

class CadenceReportSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-06-27T10:00:00Z")
  private def stats(streak: Int, last: Option[RatingChange] = None, prev: Option[RatingChange] = None) =
    RatingChangeStats(streak, windowChecks = streak + 1, windowChanges = 0, t0, t0, last, prev)

  "intervalLabel" should "render compact day/hour labels" in {
    CadenceReport.intervalLabel(2.hours) shouldBe "2h"
    CadenceReport.intervalLabel(16.hours) shouldBe "16h"
    CadenceReport.intervalLabel(2.days)  shouldBe "2d"
    CadenceReport.intervalLabel(4.days)  shouldBe "4d"
  }

  "parseKey" should "split a tmdbId-keyed dedup key and fall back for a legacy title key" in {
    CadenceReport.parseKey("mc|tmdb:42")        shouldBe ("mc", Some(42))
    CadenceReport.parseKey("imdb|Dune|2024")    shouldBe ("imdb", None)
  }

  "build" should "group by interval and order groups by interval descending (most stable first)" in {
    val records = Seq(
      "mc|tmdb:1"   -> stats(0),   // base 2h
      "imdb|tmdb:2" -> stats(6),   // capped 4d
      "fw|tmdb:3"   -> stats(1)    // 4h
    )
    val groups = CadenceReport.build(records, titleFor = id => Some(s"Film$id"))
    groups.map(_.interval) shouldBe Seq(4.days, 4.hours, 2.hours)
    groups.head.entries.map(_.title) shouldBe Seq("Film2")
  }

  it should "resolve titles via tmdbId and surface the last two changes" in {
    val last = RatingChange(t0, "7.2")
    val prev = RatingChange(t0.minusSeconds(86400), "7.0")
    val groups = CadenceReport.build(Seq("imdb|tmdb:9" -> stats(2, Some(last), Some(prev))), id => Some("Oppenheimer"))
    val e = groups.flatMap(_.entries).loneElement
    e.title      shouldBe "Oppenheimer"
    e.site       shouldBe "imdb"
    e.lastChange shouldBe Some(last)
    e.prevChange shouldBe Some(prev)
  }

  it should "fall back to tmdb:<id> when the title is unknown" in {
    val groups = CadenceReport.build(Seq("rt|tmdb:404" -> stats(0)), titleFor = _ => None)
    groups.flatMap(_.entries).loneElement.title shouldBe "tmdb:404"
  }

  it should "populate each entry's next refresh in (lastChecked, lastChecked + interval]" in {
    val e = CadenceReport.build(Seq("mc|tmdb:7" -> stats(6)), _ => Some("X")).flatMap(_.entries).loneElement
    e.interval shouldBe 4.days     // streak 6 → cap
    e.nextRefreshAt.isAfter(t0) shouldBe true
    e.nextRefreshAt.isAfter(t0.plus(java.time.Duration.ofDays(4))) shouldBe false
  }

  "nextRefreshAt" should "land on a phase boundary in (last, last+interval] and be deterministic" in {
    val interval = 8.hours
    val next = CadenceReport.nextRefreshAt("mc|tmdb:1", t0, interval)
    next.isAfter(t0)                                        shouldBe true
    next.toEpochMilli - t0.toEpochMilli                     should be <= interval.toMillis
    // A boundary: (next - phase) is a whole number of periods. Equivalently, the
    // boundary one period earlier is at-or-before `last`.
    next.minusMillis(interval.toMillis).isAfter(t0)         shouldBe false
    CadenceReport.nextRefreshAt("mc|tmdb:1", t0, interval)  shouldBe next   // deterministic
  }
}
