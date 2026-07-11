package services.movies

import clients.TmdbClient
import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{DomainEvent, ImdbIdMissing, InProcessEventBus}
import services.freshness.InMemoryFreshnessStore
import services.tasks.{DueWindow, InMemoryTaskQueue, RatingEnqueuer, RatingTasks, TaskState}
import tools.RoutingHttpFetch

import java.time.Instant
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._

/**
 * `announceResolvedNewMovie` handles a film freshly PROMOTED out of staging (vs
 * merged into an existing movie): it stamps the row's TMDB-resolution time (for
 * the first-rating-attempt delay metric), for a TMDB-only hit publishes
 * `ImdbIdMissing` to kick id recovery, and IMMEDIATELY enqueues the newcomer's
 * now-eligible rating tasks (so a newcomer's ratings don't wait for the reaper's
 * next tick — a trickle, not the old `TmdbResolved` corpus burst). A `tmdbNoMatch`
 * promotion stays silent. This pins those branches.
 */
class MovieServiceAnnounceResolvedSpec extends AnyFlatSpec with Matchers {

  private val deadTmdb = new TmdbClient(http = RoutingHttpFetch.dead("unused"), apiKey = None)

  private def fixture(): (MovieService, ListBuffer[DomainEvent], InMemoryFreshnessStore, InMemoryTaskQueue) = {
    val bus  = new InProcessEventBus()
    val seen = ListBuffer.empty[DomainEvent]
    bus.subscribe { case e => seen += e }
    val freshness = new InMemoryFreshnessStore
    val queue     = new InMemoryTaskQueue
    // The real production enqueuer over an in-memory queue — same eligibility + due
    // gate the EnrichmentReaper uses, so this exercises the actual newcomer kick.
    val enqueuer  = new RatingEnqueuer(queue, freshness, new DueWindow(4.hours))
    val service = new MovieService(
      new CaffeineMovieCache(new InMemoryMovieRepository()), bus, deadTmdb, freshness = freshness,
      enqueueNewcomerRatings = (key, record) => { enqueuer.enqueueDueFor(key, record, Instant.parse("2026-06-21T00:00:00Z")); () })
    (service, seen, freshness, queue)
  }

  private def waiting(queue: InMemoryTaskQueue): Long =
    queue.countByState().getOrElse(TaskState.Waiting, 0L)

  "announceResolvedNewMovie" should "stamp the resolution time, fire no rating event, and enqueue all four ratings for a promotion with an imdbId" in {
    val (service, seen, freshness, queue) = fixture()
    service.announceResolvedNewMovie(
      CacheKey("Kumotry", Some(2026)), MovieRecord(tmdbId = Some(1454157), imdbId = Some("tt1454157")))

    seen shouldBe empty
    freshness.lastFetchedAt(RatingTasks.tmdbResolvedAtKey(1454157)) should not be empty
    waiting(queue) shouldBe 4L // imdb + rt + mc + fw, immediately — no waiting for the reaper
  }

  it should "publish ImdbIdMissing (→ IMDb-id recovery), stamp resolution, and enqueue only the non-IMDb ratings for a promotion without an imdbId" in {
    val (service, seen, freshness, queue) = fixture()
    service.announceResolvedNewMovie(
      CacheKey("Kumotry", Some(2026)), MovieRecord(tmdbId = Some(1454157), imdbId = None))

    seen.toSeq should matchPattern { case Seq(ImdbIdMissing("Kumotry", Some(2026), _)) => }
    freshness.lastFetchedAt(RatingTasks.tmdbResolvedAtKey(1454157)) should not be empty
    waiting(queue) shouldBe 3L // rt + mc + fw now; IMDb waits for ImdbIdResolver to land the id
  }

  it should "publish ImdbIdMissing (→ id recovery) for a tmdbNoMatch promotion, but enqueue no rating tasks (no ids yet)" in {
    // TMDB found nothing, so the film has no id to query ratings against — but we
    // STILL kick the id-recovery chain (IMDb suggestion → director → Wikidata →
    // Trakt → OMDb → Wikidata-title → Cinemeta) so the TMDB-less long tail can land
    // an imdbId → rating + a resolved year, instead of waiting for the daily OMDb sweep.
    val (service, seen, freshness, queue) = fixture()
    service.announceResolvedNewMovie(
      CacheKey("Obscure Local Premiere", Some(2026)), MovieRecord(tmdbNoMatch = true))

    seen.toSeq should matchPattern { case Seq(ImdbIdMissing("Obscure Local Premiere", Some(2026), _)) => }
    waiting(queue) shouldBe 0L // no tmdbId/imdbId yet → nothing eligible; ratings follow once the id lands
  }

  it should "stay silent for a tmdbNoMatch promotion that ALREADY carries an imdbId (nothing to recover)" in {
    val (service, seen, _, queue) = fixture()
    service.announceResolvedNewMovie(
      CacheKey("Obscure Local Premiere", Some(2026)), MovieRecord(tmdbNoMatch = true, imdbId = Some("tt9999999")))

    seen shouldBe empty
    waiting(queue) shouldBe 0L
  }
}
