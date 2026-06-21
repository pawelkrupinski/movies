package services.movies

import clients.TmdbClient
import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{DomainEvent, ImdbIdMissing, InProcessEventBus}
import services.freshness.InMemoryFreshnessStore
import services.tasks.RatingTasks
import tools.RoutingHttpFetch

import scala.collection.mutable.ListBuffer

/**
 * `announceResolvedNewMovie` handles a film freshly PROMOTED out of staging (vs
 * merged into an existing movie): it stamps the row's TMDB-resolution time (for
 * the first-rating-attempt delay metric) and, for a TMDB-only hit, publishes
 * `ImdbIdMissing` to kick id recovery. Ratings come from the `EnrichmentReaper`,
 * not a per-event enqueue, so no rating event fires. A `tmdbNoMatch` promotion
 * stays silent. This pins those three branches.
 */
class MovieServiceAnnounceResolvedSpec extends AnyFlatSpec with Matchers {

  private val deadTmdb = new TmdbClient(http = RoutingHttpFetch.dead("unused"), apiKey = None)

  private def fixture(): (MovieService, ListBuffer[DomainEvent], InMemoryFreshnessStore) = {
    val bus  = new InProcessEventBus()
    val seen = ListBuffer.empty[DomainEvent]
    bus.subscribe { case e => seen += e }
    val freshness = new InMemoryFreshnessStore
    val service = new MovieService(
      new CaffeineMovieCache(new InMemoryMovieRepository()), bus, deadTmdb, freshness = freshness)
    (service, seen, freshness)
  }

  "announceResolvedNewMovie" should "stamp the resolution time and fire no rating event for a promotion with an imdbId" in {
    val (service, seen, freshness) = fixture()
    service.announceResolvedNewMovie(
      CacheKey("Kumotry", Some(2026)), MovieRecord(tmdbId = Some(1454157), imdbId = Some("tt1454157")))

    seen shouldBe empty
    freshness.lastFetchedAt(RatingTasks.tmdbResolvedAtKey(1454157)) should not be empty
  }

  it should "publish ImdbIdMissing (→ IMDb-id recovery) and stamp resolution for a promotion without an imdbId" in {
    val (service, seen, freshness) = fixture()
    service.announceResolvedNewMovie(
      CacheKey("Kumotry", Some(2026)), MovieRecord(tmdbId = Some(1454157), imdbId = None))

    seen.toSeq should matchPattern { case Seq(ImdbIdMissing("Kumotry", Some(2026), _)) => }
    freshness.lastFetchedAt(RatingTasks.tmdbResolvedAtKey(1454157)) should not be empty
  }

  it should "do nothing for a tmdbNoMatch promotion (no id to recover or query ratings against)" in {
    val (service, seen, freshness) = fixture()
    service.announceResolvedNewMovie(
      CacheKey("Obscure Local Premiere", Some(2026)), MovieRecord(tmdbNoMatch = true))

    seen shouldBe empty
    freshness.lastFetchedAt(RatingTasks.tmdbResolvedAtKey(1454157)) shouldBe empty
  }
}
