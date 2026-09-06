package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ScrapeHealth.Depth

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

  it should "never infer one from a small venue's swings" in {
    ScrapeHealth.looksPartial(knownSlots = ScrapeHealth.MinSlotsForShrinkGuard - 1, batchSlots = 1, listingIsComplete = true) shouldBe false
  }
}
