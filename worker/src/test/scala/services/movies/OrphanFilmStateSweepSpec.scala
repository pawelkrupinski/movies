package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.attempts.{AttemptOutcome, EnrichmentAttempt, InMemoryEnrichmentAttemptStore}
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.duration._

/** A film gone from the corpus kept its rating stamps and last attempts for good; the sweep deletes the
 *  TMDB-keyed ones unwritten for a month, and nothing when the corpus could not be read whole. */
class OrphanFilmStateSweepSpec extends AnyFlatSpec with Matchers {

  private val now   = Instant.parse("2026-10-04T00:00:00Z")
  private val clock = Clock.fixed(now, ZoneOffset.UTC)
  private def daysAgo(d: Int) = now.minusMillis(d.days.toMillis)

  private def stores() = {
    val freshness = new InMemoryFreshnessStore
    freshness.markFresh("imdb|tmdb:1", FreshnessKind.ImdbRating, daysAgo(60))   // live film
    freshness.markFresh("imdb|tmdb:2", FreshnessKind.ImdbRating, daysAgo(60))   // gone, old
    freshness.markFresh("imdb|tmdb:3", FreshnessKind.ImdbRating, daysAgo(5))    // gone, recent
    freshness.markFresh("imdb|Lalka|1968", FreshnessKind.ImdbRating, daysAgo(60)) // title-keyed: never ours
    freshness.markFresh("scrape|Kino Muza", FreshnessKind.CinemaScrape, daysAgo(60))
    val attempts = new InMemoryEnrichmentAttemptStore
    attempts.record("mc|tmdb:2", EnrichmentAttempt(daysAgo(60), 10, AttemptOutcome.Unchanged))
    (freshness, attempts)
  }

  "a sweep" should "delete the old rows of films the corpus no longer holds, and nothing else" in {
    val (freshness, attempts) = stores()
    new OrphanFilmStateSweep(Seq("freshness" -> freshness.retention, "attempts" -> attempts.retention),
      () => Some(Set(1)), clock).sweep() shouldBe Map("freshness" -> 1, "attempts" -> 1)
    freshness.lastFetchedAt("imdb|tmdb:2") shouldBe None
    Seq("imdb|tmdb:1", "imdb|tmdb:3", "imdb|Lalka|1968", "scrape|Kino Muza").foreach(k => freshness.lastFetchedAt(k) should not be None)
    attempts.all() shouldBe empty
  }

  it should "delete nothing when the corpus read was incomplete" in {
    val (freshness, attempts) = stores()
    new OrphanFilmStateSweep(Seq("freshness" -> freshness.retention, "attempts" -> attempts.retention), () => None, clock)
      .sweep() shouldBe empty
    freshness.lastFetchedAt("imdb|tmdb:2") should not be None
    attempts.all() should have size 1
  }

  it should "keep a row written again after the scan read it" in {
    val (freshness, _) = stores()
    val sweep = new OrphanFilmStateSweep(Seq("freshness" -> freshness.retention),
      () => { freshness.markFresh("imdb|tmdb:2", FreshnessKind.ImdbRating, now); Some(Set(1)) }, clock)
    sweep.sweep() shouldBe Map("freshness" -> 0)
    freshness.lastFetchedAt("imdb|tmdb:2") shouldBe Some(now)
  }

  "the key reader" should "name a film only by a TMDB key" in {
    OrphanFilmStateSweep.tmdbIdOf("rt|tmdb:603") shouldBe Some(603)
    OrphanFilmStateSweep.tmdbIdOf("rt|Matrix|1999") shouldBe None
    OrphanFilmStateSweep.tmdbIdOf("detail-page|x|tmdb:1") shouldBe None
  }

  "the live ids" should "come from a complete scan of the repository, and be None for an incomplete one" in {
    val repository = new InMemoryMovieRepository(Seq(("Lalka", Some(1968), models.MovieRecord(tmdbId = Some(7)))), normalizer = SingleCountryNormalizer.titleNormalizer)
    OrphanFilmStateSweep.liveTmdbIds(repository)() shouldBe Some(Set(7))
    val failing = new InMemoryMovieRepository(normalizer = SingleCountryNormalizer.titleNormalizer) { override def foreachRecordWithoutShowtimes(f: StoredMovieRecord => Unit) = tools.ScanOutcome.Incomplete(new RuntimeException("short")) }
    OrphanFilmStateSweep.liveTmdbIds(failing)() shouldBe None
  }
}
