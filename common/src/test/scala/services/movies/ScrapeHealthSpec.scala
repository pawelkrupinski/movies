package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ScrapeHealth.Depth

import scala.concurrent.duration._

class ScrapeHealthSpec extends AnyFlatSpec with Matchers {

  "the depth guard" should "reject a tick that halves a big board, and count the rejection" in {
    ScrapeHealth.depth(knownShowtimes = 100, batchShowtimes = 40, consecutiveRejections = 0) shouldBe Depth.Reject(1)
    ScrapeHealth.depth(100, 40, 2) shouldBe Depth.Reject(3)
  }

  it should "let a shrink land once it has been rejected for the whole run of ticks" in {
    // The UK incident shape: 18% of the stored showtimes, tick after tick.
    ScrapeHealth.depth(100, 18, ScrapeHealth.MaxConsecutiveDepthRejections) shouldBe Depth.AcceptDegraded(4)
  }

  it should "leave a small board alone, where a halving can be real" in {
    ScrapeHealth.depth(knownShowtimes = ScrapeHealth.MinShowtimesForDepthGuard - 1, batchShowtimes = 3, 0) shouldBe Depth.Healthy
  }

  it should "call a healthy tick healthy at the floor" in {
    ScrapeHealth.depth(100, 50, 0) shouldBe Depth.Healthy   // exactly half is not below the floor
    ScrapeHealth.depth(100, 104, 0) shouldBe Depth.Healthy
  }

  "the breadth guard" should "trust a caller that says the listing is short" in {
    ScrapeHealth.looksPartial(knownSlots = 2, batchSlots = 2, listingIsComplete = false) shouldBe true
  }

  it should "infer a partial scrape from an implausible shrink of a big board" in {
    ScrapeHealth.looksPartial(knownSlots = 20, batchSlots = 9, listingIsComplete = true) shouldBe true
    ScrapeHealth.looksPartial(knownSlots = 20, batchSlots = 10, listingIsComplete = true) shouldBe false
  }

  it should "never infer one from a small venue's ordinary swing" in {
    // 7 -> 5, a drop of 2 — the biggest UNGUARDED drop seen across 68 real small
    // venues over 2026-09-03..09-08 was 2 (Kino Forum, 8 -> 6).
    ScrapeHealth.looksPartial(knownSlots = ScrapeHealth.MinSlotsForShrinkGuard - 1, batchSlots = 5, listingIsComplete = true) shouldBe false
  }

  it should "still catch a small venue's near-total collapse" in {
    // CineStars Hood River, 2026-09-07: 7 slots -> 1. MinSlotsForShrinkGuard used to
    // exempt this outright (7 < 8) regardless of the ratio, and scrape-prune deleted
    // six still-screening films. The ratio here (1/7 = 0.14) would have tripped the
    // guard easily had the venue been one slot bigger.
    ScrapeHealth.looksPartial(knownSlots = 7, batchSlots = 1, listingIsComplete = true) shouldBe true
  }

  it should "trip exactly at the absolute-drop floor, and not one slot short of it" in {
    // knownSlots=7: a drop of 3 (-> 4 remaining) is still under the floor; a drop of
    // 4 (-> 3 remaining) meets it. `>=`, not `>`, matching the ratio test's own
    // inclusive boundary above ("exactly half is not below the floor" reads the
    // other way because that one is a `<` against the ratio, not a `>=` against
    // the drop — both boundaries land ON the healthy side at the last healthy value).
    ScrapeHealth.looksPartial(knownSlots = 7, batchSlots = 4, listingIsComplete = true) shouldBe false
    ScrapeHealth.looksPartial(knownSlots = 7, batchSlots = 3, listingIsComplete = true) shouldBe true
  }

  "maxRejectionsFor" should "leave Poland's own cadence at the full three-tick grace" in {
    ScrapeHealth.maxRejectionsFor(60.minutes) shouldBe 3
  }

  it should "cap the slower countries at one tick, so the hold can't outlast a shallow venue's runway" in {
    // es/Multicines Zamora (420min cadence): held three rejected ticks (21h) while
    // its SensaCine-advertised 3-day window ran dry after 13h30m — the guard's own
    // grace outlasted the corpus it was protecting.
    ScrapeHealth.maxRejectionsFor(420.minutes) shouldBe 1  // es + uk
    ScrapeHealth.maxRejectionsFor(600.minutes) shouldBe 1  // de
    ScrapeHealth.maxRejectionsFor(840.minutes) shouldBe 1  // us
  }

  it should "never drop below one tick, however long the cadence" in {
    ScrapeHealth.maxRejectionsFor(30.hours) shouldBe 1
  }

  it should "let a caller lower the depth guard's own grace to match" in {
    // Same shape as the sustained-shrink spec above, but capped at 1 instead of the
    // default 3: the SECOND rejected tick (not the fourth) accepts the degraded data.
    ScrapeHealth.depth(100, 40, consecutiveRejections = 0, maxConsecutiveRejections = 1) shouldBe Depth.Reject(1)
    ScrapeHealth.depth(100, 40, consecutiveRejections = 1, maxConsecutiveRejections = 1) shouldBe Depth.AcceptDegraded(2)
  }
}
