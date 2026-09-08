package services.tasks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.freshness.InMemoryFreshnessStore

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.duration._

/**
 * `ScrapeFreshnessPolicy` on its own — `ScrapeTasksSpec` / `ChunkScrapeFlowSpec`
 * already pin the freshness-stamping rule this class exists for (success stamps,
 * failure holds within budget); this spec covers the ADDED responsibility, feeding
 * a landed scrape's remaining runway into the shared `VenueCadenceStore` — see
 * `VenueScrapeCadence`.
 */
class ScrapeFreshnessPolicySpec extends AnyFlatSpec with Matchers {

  private val clock = Clock.fixed(Instant.parse("2026-09-08T10:00:00Z"), ZoneOffset.UTC)
  private val venueKey = "scrape|Cinema One Antlers"

  "succeeded" should "leave a venue on the country default when no venueCadence store is wired" in {
    // The default constructor param — every existing caller/test that builds this
    // class without a venueCadence keeps behaving exactly as before.
    val policy = new ScrapeFreshnessPolicy(new InMemoryFreshnessStore, clock = clock)
    noException should be thrownBy policy.succeeded(venueKey, Some(10.minutes))
  }

  it should "shorten the venue's own cadence when a thin runway is reported" in {
    val store  = new VenueCadenceStore(countryDefault = 14.hours)
    val policy = new ScrapeFreshnessPolicy(new InMemoryFreshnessStore, clock = clock, venueCadence = Some(store))
    policy.succeeded(venueKey, Some(7.hours))
    store.periodFor(venueKey) shouldBe 3.5.hours
  }

  it should "leave the cadence untouched when the caller has no horizon to report" in {
    // The GoneUpstream skip path: no listing was fetched at all, so there is
    // nothing to measure. Must not silently reset a venue's already-shortened
    // cadence back to the country default.
    val store  = new VenueCadenceStore(countryDefault = 14.hours)
    val policy = new ScrapeFreshnessPolicy(new InMemoryFreshnessStore, clock = clock, venueCadence = Some(store))
    store.record(venueKey, remainingHorizon = 7.hours)
    policy.succeeded(venueKey)
    store.periodFor(venueKey) shouldBe 3.5.hours
  }

  "skipped" should "never touch the venue cadence, matching succeeded's own no-horizon case" in {
    val store  = new VenueCadenceStore(countryDefault = 14.hours)
    val policy = new ScrapeFreshnessPolicy(new InMemoryFreshnessStore, clock = clock, venueCadence = Some(store))
    store.record(venueKey, remainingHorizon = 7.hours)
    policy.skipped(venueKey)
    store.periodFor(venueKey) shouldBe 3.5.hours
  }
}
