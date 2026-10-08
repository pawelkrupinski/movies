package services.movies

import tools.SpecClock.given

import services.movies.SingleCountryNormalizer.titleNormalizer

import clients.TmdbClient
import models.{Country, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{DomainEvent, ImdbIdMissing, InProcessEventBus}
import services.freshness.InMemoryFreshnessStore
import services.tasks.{DueWindow, RatingSources, InMemoryTaskQueue, RatingEnqueuer, RatingTasks, TaskState}
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
      new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned), bus, deadTmdb, freshness = freshness,
      enqueueNewcomerRatings = (key, record) => { enqueuer.enqueueDueFor(key, record, Instant.parse("2026-06-21T00:00:00Z")); () },
      forceRatingRefresh = (key, record) => { enqueuer.enqueueDueFor(key, record, Instant.parse("2026-06-21T00:00:00Z"), force = true); () }, clock = _root_.tools.SpecClock.Pinned)
    (service, seen, freshness, queue)
  }

  private def waiting(queue: InMemoryTaskQueue): Long =
    queue.countByState().getOrElse(TaskState.Waiting, 0L)

  "announceResolvedNewMovie" should "stamp the resolution time, fire no rating event, and enqueue all four ratings for a promotion with an imdbId" in {
    val (service, seen, freshness, queue) = fixture()
    service.announceResolvedNewMovie(
      CacheKey("Kumotry", Some(2026), titleNormalizer), MovieRecord(tmdbId = Some(1454157), imdbId = Some("tt1454157")))

    seen shouldBe empty
    freshness.lastFetchedAt(RatingTasks.tmdbResolvedAtKey(1454157)) should not be empty
    waiting(queue) shouldBe 4L // imdb + rt + mc + fw, immediately — no waiting for the reaper
  }

  it should "publish ImdbIdMissing (→ IMDb-id recovery), stamp resolution, and enqueue only the non-IMDb ratings for a promotion without an imdbId" in {
    val (service, seen, freshness, queue) = fixture()
    service.announceResolvedNewMovie(
      CacheKey("Kumotry", Some(2026), titleNormalizer), MovieRecord(tmdbId = Some(1454157), imdbId = None))

    seen.toSeq should matchPattern { case Seq(ImdbIdMissing("Kumotry", Some(2026), _)) => }
    freshness.lastFetchedAt(RatingTasks.tmdbResolvedAtKey(1454157)) should not be empty
    waiting(queue) shouldBe 3L // rt + mc + fw now; IMDb waits for ImdbIdResolver to land the id
  }

  it should "kick no title-search IMDb recovery for a tmdbNoMatch promotion, and enqueue nothing it has no id for" in {
    // A film TMDB has no record of takes its IMDb id from the identity resolver's fallback source
    // (`ResolverDecision.fallback`), on every fact it publishes — never from the title-search ladder,
    // which guessed PL "Lalka" (2026) the 1968 film's id and its 6.9.
    val (service, seen, _, queue) = fixture()
    service.announceResolvedNewMovie(
      CacheKey("Obscure Local Premiere", Some(2026), titleNormalizer), MovieRecord(tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy)))

    seen shouldBe empty
    waiting(queue) shouldBe 0L // no tmdbId/imdbId → nothing eligible
  }

  it should "enqueue the IMDb rating of a tmdbNoMatch promotion the fallback source gave an imdbId, at once" in {
    val (service, seen, _, queue) = fixture()
    service.announceResolvedNewMovie(
      CacheKey("Obscure Local Premiere", Some(2026), titleNormalizer), MovieRecord(tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy), imdbId = Some("tt9999999")))

    seen shouldBe empty
    waiting(queue) shouldBe 1L // IMDb's rating, off the fallback's id
  }

  "announceReidentified" should "re-fetch every rating of a film an identity projection rebuilt under a new TMDB answer, though its title rated it minutes before" in {
    // Run 36771862724: 'Afrykanska Przygoda' was rated while TMDB-less (stamps under its title key), then
    // matched on the next projection, which rebuilt its record without those ratings. The title-keyed
    // stamps still read fresh, so nothing re-rated it until the next day's due window — a day-one card
    // with no ratings that the next day filled in.
    val (service, _, freshness, queue) = fixture()
    val key = CacheKey("Afrykanska Przygoda", Some(2007), titleNormalizer)
    RatingSources.forCountry(Country.default).foreach(s =>
      freshness.markFresh(RatingTasks.dedupKey(s.kind, key), s.kind, Instant.parse("2026-06-20T23:59:00Z")))

    service.announceReidentified(key, MovieRecord(tmdbId = Some(435263), imdbId = Some("tt1099921")))

    waiting(queue) shouldBe 4L
  }
}
