package services.movies

import clients.TmdbClient
import models.{Filmweb, Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{InProcessEventBus, MovieDetailsComplete}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.resolution.TmdbAttempt
import tools.GetOnlyHttpFetch

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

/**
 * A queued re-resolve must not be short-circuited by the very miss it was queued
 * to overcome.
 *
 * `MergeRetrigger` enqueues `ResolveTmdb` when an enrichment supplies a new
 * resolution input — a Filmweb-discovered `originalTitle` is the case it exists
 * for, and the one that lets Filmweb crack a film TMDB's own search missed. That
 * title is no EVIDENCE (a derived slot is never what a cinema published), but it
 * is a legitimate SEARCH input, so the miss's fingerprint covers the derived search
 * terms too: once Filmweb supplies one, the fingerprint of what would be searched
 * differs from the fingerprint the miss was reached on, and the row re-opens.
 *
 * This used to need the negative marker cleared at the retrigger site, and a
 * guard on the site to keep that edge-triggered. Now nothing clears anything.
 */
class RememberedMissRetriggerSpec extends AnyFlatSpec with Matchers {

  private val Title = "Mistyczka"
  private val Year  = Some(2026)

  private class CountingTmdb extends GetOnlyHttpFetch {
    val searches = new AtomicInteger
    override def get(url: String): String = {
      if (url.contains("/search/movie")) searches.incrementAndGet()
      """{"results":[]}"""
    }
  }

  "a remembered miss" should "re-open when a Filmweb-supplied original title changes what would be searched" in {
    val http    = new CountingTmdb
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(), normalizer = titleNormalizer)
    val bus     = new InProcessEventBus()
    val service = new MovieService(cache, bus, new TmdbClient(http = http, apiKey = Some("stub")))
    bus.subscribe(service.onMovieDetailsComplete)

    val key = cache.keyOf(Title, Year)
    val row = MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(title = Some(Title))))
    cache.put(key, row)

    // TMDB looked with the cinema's title alone and found nothing.
    bus.publish(MovieDetailsComplete(Title, Year, None, None))
    service.drain()
    val miss = cache.get(key).flatMap(_.tmdbAttempt)
    miss should not be empty
    val searchesSoFar = http.searches.get()
    searchesSoFar should be > 0

    // The same inputs again: the miss stands, TMDB is not asked.
    bus.publish(MovieDetailsComplete(Title, Year, None, None))
    service.drain()
    http.searches.get() shouldBe searchesSoFar

    // Filmweb supplies an original title — a new search term, not a cinema hint.
    cache.putIfPresent(key, r => r.copy(data = r.data + ((Filmweb: Source) -> SourceData(originalTitle = Some("The Mystic")))))
    bus.publish(MovieDetailsComplete(Title, Year, None, None))
    service.drain()
    http.searches.get() should be > searchesSoFar
    cache.get(key).flatMap(_.tmdbAttempt) should not be miss
  }

  it should "stand when nothing a search reads has changed" in {
    val key  = CacheKey(Title, Year, titleNormalizer)
    val at   = Instant.parse("2026-09-06T10:00:00Z")
    val row  = MovieRecord(data = Map[Source, SourceData](Helios -> SourceData(title = Some(Title))))
    val miss = TmdbAttempt.on(row.evidence, row.resolverOriginalTitles, at)
    miss.covers(TmdbAttempt.fingerprint(row.evidence, row.resolverOriginalTitles), at.plusSeconds(3600)) shouldBe true
    miss.covers(TmdbAttempt.fingerprint(row.evidence, Seq("The Mystic")), at.plusSeconds(3600)) shouldBe false
    miss.covers(TmdbAttempt.fingerprint(row.evidence, Nil), at.plus(TmdbAttempt.RetryAfter).plusSeconds(1)) shouldBe false
    TmdbAttempt.Legacy.covers(TmdbAttempt.fingerprint(row.evidence, Nil), at) shouldBe false
    key.cleanTitle shouldBe Title
  }
}
