package services.movies

import clients.TmdbClient
import models.{Helios, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.readmodel.{InMemoryReadModelRepository, ReadModelProjector}
import services.resolution.TmdbAttempt
import tools.GetOnlyHttpFetch

import java.time.{Instant, LocalDateTime}
import java.util.concurrent.atomic.AtomicInteger

/**
 * A re-try of a no-match row must not take the row off the site while TMDB is asked again.
 *
 * `UnresolvedTmdbReaper` re-tries every unresolved row once a day through `retryResolve`
 * (and the operator's `RefreshAllTmdb` through `retryUnresolvedTmdb`). Both used to CLEAR the
 * row's `tmdbAttempt` before dispatching, so the search would not be short-circuited by the
 * very miss it was re-trying — but a row without a conclusion fails `readyToProject`, and the
 * projector retires its card the moment the change stream shows it (prod 2026-09-23,
 * `reason=stream-row-unready`: Met Opera events off ~200 US city pages). The re-try now says
 * "look past the remembered miss" to the lookup itself; the stored miss stands until a new
 * answer replaces it.
 */
class RetryResolveKeepsRowReadySpec extends AnyFlatSpec with Matchers {

  // The service and cache run at one fixed instant; the row's showtime and its remembered
  // miss are placed relative to it.
  private val specClock = java.time.Clock.fixed(Instant.parse("2026-06-01T10:00:00Z"), java.time.ZoneOffset.UTC)
  private val Title     = "Klasyka Bajek Polskich"
  private val Row   = MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(
    title = Some(Title), showtimes = Seq(Showtime(LocalDateTime.now(specClock).plusDays(2).withNano(0), None)))))
  // A miss on exactly the row's current inputs, recent enough to stand: a plain dispatch
  // would not search again, so only the re-try's own say-so can make it look.
  private val FirstMiss = TmdbAttempt.on(Row.evidence, Row.resolverOriginalTitles, specClock.instant().minusSeconds(3600))

  private class NoMatchTmdb extends GetOnlyHttpFetch {
    val searches = new AtomicInteger
    override def get(url: String): String = {
      if (url.contains("/search/movie")) searches.incrementAndGet()
      """{"results":[]}"""
    }
  }

  private final class Fixture {
    val http       = new NoMatchTmdb
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val readModel  = new InMemoryReadModelRepository()
    val projector  = new ReadModelProjector(repository, readModel, readModel, clock = specClock)
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = specClock)
    val service    = new MovieService(cache, new InProcessEventBus(), new TmdbClient(http = http, apiKey = Some(settings.TmdbApiKey("stub"))), clock = specClock)
    val key        = cache.keyOf(Title, None)
    // Every row state the change stream shows, in order — what the projector sees.
    val streamed   = scala.collection.mutable.Buffer.empty[Boolean]
    repository.watchChanges(r => if (r.cacheKey(titleNormalizer) == key) streamed.synchronized(streamed += r.record.readyToProject), _ => ())
    repository.watchUpserts(projector.onMovieUpsert)

    cache.put(key, Row.copy(tmdbAttempt = Some(FirstMiss)))
    val card: String = cache.idOf(key).getOrElse(fail("the seeded row has no id")).value
    readModel.findAllMovieIds() should contain(card)
    streamed.clear()
  }

  "retryResolve" should "keep a no-match row ready, and its card served, while TMDB answers no match again" in {
    val f = new Fixture
    f.service.retryResolve(Title, None)
    f.service.drain()

    withClue("premise — the retry looked past the remembered miss and asked TMDB: ")(f.http.searches.get() should be > 0)
    withClue("the new miss replaces the old one: ")(f.cache.get(f.key).flatMap(_.tmdbAttempt).map(_.at) should not be Some(FirstMiss.at))
    withClue("the change stream showed the row unready, which retires its card: ")(f.streamed should not contain false)
    f.readModel.findAllMovieIds() should contain(f.card)
  }

  "retryUnresolvedTmdb" should "keep every no-match row ready while it re-asks TMDB" in {
    val f = new Fixture
    f.service.retryUnresolvedTmdb()
    f.service.drain()

    withClue("premise — the retry looked past the remembered miss and asked TMDB: ")(f.http.searches.get() should be > 0)
    withClue("the change stream showed the row unready, which retires its card: ")(f.streamed should not contain false)
    f.readModel.findAllMovieIds() should contain(f.card)
  }

  "a plain re-dispatch" should "still honour the remembered miss" in {
    val f = new Fixture
    f.service.resolveTmdbOnce(Title, None, None, None, services.tasks.ResolveMode.Normal) shouldBe true
    f.http.searches.get() shouldBe 0
    f.cache.get(f.key).flatMap(_.tmdbAttempt) shouldBe Some(FirstMiss)
  }
}
