package services.movies

import models.{Filmweb, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * A queued re-resolve must not be short-circuited by the very negative marker it
 * was queued to overcome.
 *
 * `MergeRetrigger` enqueues `ResolveTmdb` when an enrichment supplies a new
 * resolution input — a Filmweb-discovered `originalTitle` is the case it exists
 * for, and the one that lets Filmweb crack a film TMDB's own search missed. But
 * the handler then asks `MovieService.needsTmdbResolution`, which short-circuits
 * on `isNegative` unless the event carries a CINEMA-published hint. A
 * Filmweb-supplied title is not one, so the task ran and did nothing, and the new
 * evidence sat unused until the 24h negative TTL expired.
 *
 * Clearing the marker where the retrigger is decided is what keeps this safe: the
 * decision is EDGE-triggered — it fires only when an input actually changed — so
 * it cannot re-arm every tick the way a "bypass whenever a derived title exists"
 * rule inside `needsTmdbResolution` would. That level-triggered shape is exactly
 * the re-divert churn `MixedFilmDetector`'s corroboration guard was added to stop.
 */
class NegativeCacheRetriggerSpec extends AnyFlatSpec with Matchers {

  private val filmKey = CacheKey("Mistyczka", Some(2026), titleNormalizer)

  private def cacheWithMissedRow = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(), normalizer = titleNormalizer)
    cache.put(filmKey, MovieRecord())       // unresolved: TMDB looked and found nothing…
    cache.markMissing(filmKey)              // …so the miss is remembered for 24h
    cache
  }

  "a resolve retrigger" should "clear the negative marker it would otherwise be short-circuited by" in {
    val cache  = cacheWithMissedRow
    val before = cache.get(filmKey).getOrElse(fail("row missing"))
    cache.isNegative(filmKey) shouldBe true

    // Filmweb supplies an original title — a new resolution input, so
    // `MergeRetrigger` asks for a ResolveTmdb.
    val after = before.copy(data = before.data +
      ((Filmweb: Source) -> SourceData(originalTitle = Some("Mistyczka"))))
    cache.putIfPresent(filmKey, _ => after)
    cache.retriggerAfterEnrichment(filmKey, before, after)

    cache.isNegative(filmKey) shouldBe false
  }

  it should "leave the marker alone when nothing a resolution reads changed" in {
    // Level-triggered would clear here too, and then every tick — the churn guard.
    val cache  = cacheWithMissedRow
    val before = cache.get(filmKey).getOrElse(fail("row missing"))

    cache.retriggerAfterEnrichment(filmKey, before, before)

    cache.isNegative(filmKey) shouldBe true
  }
}
