package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.InMemoryStagingRepository

/**
 * The index that shadows the corpus must never disagree with the corpus.
 *
 * [[CorpusIndex]] answers, in O(1), the questions `recordCinemaScrape` used to answer by
 * walking every row. That is only sound while every write to `positive` updates it, and
 * a write that forgets to is SILENT: nothing throws, nothing logs, and the first symptom
 * is a film the cache no longer recognises re-diverting into staging on every tick —
 * the served-count flap (Trójmiasto / GCF) the widened divert gate exists to prevent, or
 * a prune that stops seeing a venue's slots and leaves a closed film on the site forever.
 * By the time either is visible it looks like a data problem, not an index problem.
 *
 * So this replays a sequence that exercises every funnel — the scrape write, the
 * `putIfPresent` compute, the prune's slot drop, an invalidate, a re-key, a rehydrate —
 * and after each step asserts the incremental index equals one rebuilt from the rows.
 * Rebuilt-from-rows is the definition; incremental is the optimisation; the spec is the
 * bridge, and it fails on the STEP that broke rather than wherever the wrong answer
 * eventually surfaced.
 */
class CorpusIndexConsistencySpec extends AnyFlatSpec with Matchers {

  private val normalizer     = SingleCountryNormalizer.titleNormalizer
  private val cinema: Cinema = KinoMuza
  private val other: Cinema  = Multikino

  private def scrape(title: String, cin: Cinema, year: Option[Int] = Some(2026)): CinemaMovie =
    CinemaMovie(Movie(title = title, releaseYear = year),
      cin, posterUrl = None, filmUrl = None, synopsis = None,
      cast = Nil, director = Nil, showtimes = Nil)

  private def fixture = {
    val staging = new InMemoryStagingRepository(normalizer = normalizer)
    val repo    = new InMemoryMovieRepository(normalizer = normalizer)
    (new CaffeineMovieCache(repo, staging = Some(staging), normalizer = normalizer), staging)
  }

  /** The whole point: after `step`, does the index still describe the rows? */
  private def stillAgrees(cache: CaffeineMovieCache, step: String): Unit =
    withClue(s"after $step, the incremental index no longer matches one rebuilt from the rows: ") {
      cache.indexSnapshot shouldBe cache.rowsRebuiltIndexSnapshot
    }

  "the corpus index" should "match the rows through a whole scrape / fold / prune cycle" in {
    val (cache, _) = fixture

    cache.recordCinemaScrape(cinema, Seq(scrape("First Film", cinema), scrape("Second Film", cinema)))
    stillAgrees(cache, "the first scrape")

    // A second venue holding one of the same films — the (cinema, slot) index is
    // per-venue, so this is where a key held by two rows or two venues shows up.
    cache.recordCinemaScrape(other, Seq(scrape("Second Film", other), scrape("Third Film", other)))
    stillAgrees(cache, "a second venue's scrape")

    // A re-scrape that DROPS a film: the prune path, which rewrites rows through
    // `putIfPresent` rather than a plain put.
    cache.recordCinemaScrape(cinema, Seq(scrape("First Film", cinema)))
    stillAgrees(cache, "a prune that drops a venue's slot")

    // An enrichment-shaped update: `putIfPresent` writes through Caffeine's own compute,
    // the one funnel that cannot go through `store`.
    cache.putIfPresent(CacheKey("First Film", Some(2026), normalizer),
      r => r.copy(tmdbId = Some(603), data = r.data + (Tmdb -> SourceData(title = Some("The Matrix")))))
    stillAgrees(cache, "an enrichment update through putIfPresent")

    // A concluded row contributes ALIASES; concluding one must add them and dropping it
    // must take them away again, refcounted.
    cache.putIfPresent(CacheKey("Third Film", Some(2026), normalizer), _.copy(tmdbNoMatch = true))
    stillAgrees(cache, "concluding a row")

    cache.invalidate(CacheKey("Third Film", Some(2026), normalizer))
    stillAgrees(cache, "invalidating a row")

    // Rehydrate replaces the whole corpus from the repository — puts for what is there,
    // evictions for what is not.
    cache.rehydrate()
    stillAgrees(cache, "a rehydrate")
  }

  /** The refcount, on its own: two rows offering the same alias, one removed. A set
   *  rather than a count would un-know an alias the surviving row still carries, and the
   *  film listed under it would incubate as a newcomer all over again. */
  it should "keep an alias known while any concluded row still carries it" in {
    val (cache, _) = fixture
    val shared     = "Shared Alias"
    Seq(2024, 2026).foreach { year =>
      cache.put(CacheKey(s"Film $year", Some(year), normalizer),
        MovieRecord(tmdbId = Some(year),
          data = Map[Source, SourceData](
            (cinema: Source) -> SourceData(title = Some(s"Film $year")),
            Tmdb            -> SourceData(title = Some(shared)))))
    }
    stillAgrees(cache, "two rows sharing an alias")
    cache.invalidate(CacheKey("Film 2024", Some(2024), normalizer))
    stillAgrees(cache, "dropping one of them")
  }
}
