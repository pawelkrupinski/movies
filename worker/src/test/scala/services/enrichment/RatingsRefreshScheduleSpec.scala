package services.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.TmdbClient
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}
import tools.RealHttpFetch

/**
 * Pins the rating-refresh cadence: the four `*Ratings` walks each run every
 * 4h, staggered ~1h apart so only one walks the cache per hour (see the note
 * on `ImdbRatings`). Without this, a regression back to the old "every 1h, all
 * four within 45s of each other" cadence would slip through silently — the
 * scheduling literals live in four separate files and nothing else checks them.
 *
 * Only the schedule fields are read; no refresh runs, so the real HTTP clients
 * are never exercised (a `RealHttpFetch` that's never called is fine).
 */
class RatingsRefreshScheduleSpec extends AnyFlatSpec with Matchers {

  private def refreshers(): Seq[PeriodicCacheRefresher] = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val http  = new RealHttpFetch
    val tmdb  = new TmdbClient(http, apiKey = None)
    Seq(
      new ImdbRatings(cache, new ImdbClient(http)),
      new RottenTomatoesRatings(cache, tmdb, new RottenTomatoesClient(http)),
      new MetascoreRatings(cache, tmdb, new MetacriticClient(http)),
      new FilmwebRatings(cache, tmdb, new FilmwebClient(http))
    )
  }

  "the rating refreshers" should "all run every 4 hours" in {
    refreshers().foreach(_.refreshHours shouldBe 4L)
  }

  it should "stagger their first runs ~1h apart across the 4h cycle" in {
    val delays = refreshers().map(_.startupDelaySeconds).sorted

    // Four distinct offsets — the first at 1h (nothing runs in the first
    // hour), the last no later than the 4h cycle length.
    delays.distinct.size shouldBe 4
    delays.foreach(d => d should (be >= 3600L and be <= 4L * 3600))
    // Consecutive offsets ~1h apart.
    delays.sliding(2).foreach { case Seq(a, b) => (b - a) should be >= 3000L }
  }
}
