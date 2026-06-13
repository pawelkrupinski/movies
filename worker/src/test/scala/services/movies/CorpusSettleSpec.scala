package services.movies

import clients.TmdbClient
import models.{Helios, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import tools.GetOnlyHttpFetch

/**
 * Regression for the stranded "Dzień objawienia" duplicate: a film reported by
 * some cinemas WITH a release year and by others WITHOUT one produces two rows —
 * `(title, Some(2026))` and `(title, None)`. TMDB resolves the yeared row;
 * the yearless row stays `tmdbId=null` (its enrichment redirects onto the
 * already-resolved sibling via `canonicalKeyFor`/`hasResolvedSiblingByTitle`,
 * so it never gets its own id), and the `put` fold gate can't merge a row that
 * carries no tmdbId. The pair is therefore stuck — two cards for one film.
 *
 * `MovieCache.canonicalizeBySanitize` collapses exactly this (same normalised
 * title, a single distinct tmdbId across the group → union onto the yeared key),
 * but for a long time it was only ever called from the fixture test harness —
 * never in production — so the suite was green while the live corpus kept the
 * dup forever. `MovieService.settle()` is the production caller; this pins that
 * it collapses the pair.
 */
class CorpusSettleSpec extends AnyFlatSpec with Matchers {

  private object NoNetwork extends GetOnlyHttpFetch {
    override def get(url: String): String = throw new RuntimeException(s"settle() must not hit the network: $url")
  }

  private def service(cache: MovieCache): MovieService =
    new MovieService(cache, new InProcessEventBus, new TmdbClient(NoNetwork, apiKey = None))

  private def slot(cinema: models.Cinema): (Source, SourceData) =
    (cinema: Source) -> SourceData(title = Some("Dzień objawienia"))

  "MovieService.settle" should "collapse a yearless unresolved row into its resolved yeared sibling" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo)

    // The resolved yeared row (Helios-class cinemas reported releaseYear=2026,
    // TMDB resolved it) …
    cache.put(cache.keyOf("Dzień objawienia", Some(2026)),
      MovieRecord(tmdbId = Some(1275779), imdbId = Some("tt15047880"), data = Map(slot(Helios))))
    // … and the yearless, still-unresolved row (Multikino-class cinemas report
    // no year; its own enrichment was suppressed by the resolved sibling).
    cache.put(cache.keyOf("Dzień objawienia", None),
      MovieRecord(tmdbId = None, data = Map(slot(Multikino))))

    // Stuck: two rows for one film — the bug the user sees.
    cache.snapshot().map(r => (r.title, r.year)).toSet shouldBe
      Set(("Dzień objawienia", Some(2026)), ("Dzień objawienia", None))

    service(cache).settle()

    val rows = cache.snapshot()
    withClue(s"expected ONE row after settle, got ${rows.map(r => (r.title, r.year))}\n") {
      rows.size shouldBe 1
    }
    rows.head.year   shouldBe Some(2026)          // year-bearing key wins
    rows.head.record.tmdbId shouldBe Some(1275779) // enrichment preserved
    rows.head.record.cinemaData.keySet shouldBe Set(Helios, Multikino) // both cinemas' slots unioned
  }
}
