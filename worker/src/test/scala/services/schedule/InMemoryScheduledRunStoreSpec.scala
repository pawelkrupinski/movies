package services.schedule

import java.time.{Duration, Instant}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The in-memory claims behave as Mongo's (`ScheduledRunStoreIntegrationSpec`): once per occurrence, gone 48 h on. */
class InMemoryScheduledRunStoreSpec extends AnyFlatSpec with Matchers {

  "InMemoryScheduledRunStore.claim" should "grant an occurrence once, and distinct occurrences independently" in {
    val store = new InMemoryScheduledRunStore
    store.claim("scrape@1") shouldBe true
    store.claim("scrape@1") shouldBe false
    store.claim("scrape@2") shouldBe true
    store.claimedIds shouldBe Set("scrape@1", "scrape@2")
  }

  it should "let a claim go 48 hours after its clock stamped it, as the TTL index deletes it" in {
    val clock = new tools.MutableClock(Instant.parse("2026-11-26T12:00:00Z"))
    val store = new InMemoryScheduledRunStore(clock)
    store.claim("scrape@1") shouldBe true
    clock.advance(Duration.ofHours(48).minusSeconds(1))
    store.claimedIds shouldBe Set("scrape@1")
    clock.advance(Duration.ofSeconds(1))
    store.claimedIds shouldBe empty
  }
}
