package services.freshness

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

class FreshnessStoreSpec extends AnyFlatSpec with Matchers {
  import FreshnessKind._

  private val t0 = Instant.parse("2026-06-07T12:00:00Z")

  "isFresh" should "be false for a key never marked" in {
    val store = new InMemoryFreshnessStore
    store.isFresh("scrape|kino-x", CinemaScrape, t0) shouldBe false
  }

  it should "be true within the kind's TTL and false past it" in {
    val store = new InMemoryFreshnessStore
    store.markFresh("scrape|kino-x", CinemaScrape, t0) // 15min TTL
    store.isFresh("scrape|kino-x", CinemaScrape, t0.plusSeconds(14 * 60)) shouldBe true
    store.isFresh("scrape|kino-x", CinemaScrape, t0.plusSeconds(16 * 60)) shouldBe false
  }

  it should "use the per-kind TTL — a 4h rating stays fresh long after a 15min scrape would be stale" in {
    val store = new InMemoryFreshnessStore
    store.markFresh("imdb|movie", ImdbRating, t0) // 4h TTL
    store.isFresh("imdb|movie", ImdbRating, t0.plus(3, java.time.temporal.ChronoUnit.HOURS)) shouldBe true
    store.isFresh("imdb|movie", ImdbRating, t0.plus(5, java.time.temporal.ChronoUnit.HOURS)) shouldBe false
  }

  it should "treat a permanent kind (TMDB resolve) as fresh forever once stamped" in {
    val store = new InMemoryFreshnessStore
    store.markFresh("tmdb|movie", TmdbResolve, t0)
    store.isFresh("tmdb|movie", TmdbResolve, t0.plus(3650, java.time.temporal.ChronoUnit.DAYS)) shouldBe true
  }

  "lastFetchedAt" should "return the recorded instant" in {
    val store = new InMemoryFreshnessStore
    store.lastFetchedAt("k") shouldBe None
    store.markFresh("k", DetailEnrich, t0)
    store.lastFetchedAt("k") shouldBe Some(t0)
  }

  "Freshness.ttlFor" should "give 4h for every rating source and a permanent (None) TMDB resolve" in {
    Freshness.ttlFor(ImdbRating)    shouldBe Some(4.hours)
    Freshness.ttlFor(FilmwebRating) shouldBe Some(4.hours)
    Freshness.ttlFor(RtRating)      shouldBe Some(4.hours)
    Freshness.ttlFor(McRating)      shouldBe Some(4.hours)
    Freshness.ttlFor(CinemaScrape)  shouldBe Some(15.minutes)
    Freshness.ttlFor(DetailEnrich)  shouldBe Some(6.hours)
    Freshness.ttlFor(TmdbResolve)   shouldBe None
  }

  "FreshnessKind labels" should "be unique and round-trip via byLabel" in {
    FreshnessKind.all.map(_.label).distinct should have size FreshnessKind.all.size
    FreshnessKind.all.foreach(k => FreshnessKind.byLabel(k.label) shouldBe Some(k))
  }
}
