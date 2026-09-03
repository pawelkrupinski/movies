package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.{InMemoryStagingRepository, StagingRecord}
import services.titlerules.TitleRuleSet

/**
 * The cost of scraping ONE venue must not grow with how many films the corpus already
 * holds.
 *
 * THE BUG THIS PINS. `recordCinemaScrape` answered four questions — is this title
 * known, is it a known film's alias, does a row already hold this cinema's slot, which
 * rows share this sanitized title — by walking the WHOLE corpus and building a fresh
 * Set/Map each time, plus two guard walks and a prune walk either side. Six-plus full
 * passes, per venue. In a scrape tick that is a loop over every venue, and the corpus
 * grows as the venues land, so a fixed-size chunk of venues costs linearly more the
 * further into the tick it runs. The United States convergence leg measured it on
 * 2026-08-31 over 4205 venues in chunks of 420: 531s for the first chunk, 4281s for the
 * eighth — 8x for identical work — and the leg never finished a single tick in 5h15m.
 *
 * WHY IT IS COUNTED AND NOT TIMED. A wall-clock assertion on a shared CI runner is a
 * flake generator, and it would fail for reasons that have nothing to do with this. The
 * defect is WORK, so the test counts work: `sanitize` is called once per row-slot by
 * every one of those corpus walks, so its call count is a direct, deterministic probe of
 * whether the method is touching rows it has no business touching. Against a 10x corpus
 * it was ~10x the calls; it is now the same number, because the only titles the method
 * still sanitizes are the ones the venue itself just reported.
 *
 * Sanitize is memoized inside `TitleNormalizer`, so this counts CALLS, not misses — the
 * per-row hash lookup is the cheap part of the walk it stands in for, not the expensive
 * part. It is a proxy for the traversal, and it moves with it.
 */
class ScrapeCostIndependentOfCorpusSpec extends AnyFlatSpec with Matchers {

  /** Counts `sanitize`, which every per-row corpus walk calls once per slot. */
  private class CountingNormalizer(rules: TitleRuleSet) extends TitleNormalizer(rules) {
    var calls = 0
    override def sanitize(title: String): String = { calls += 1; super.sanitize(title) }
  }

  private val cinema: Cinema = KinoMuza
  /** A second venue, so the corpus rows carry slots of a cinema OTHER than the one being
   *  scraped — the walks paid for those too, and a per-cinema index must not. */
  private val elsewhere: Cinema = Multikino

  /** A resolved row as a warm corpus holds it: a TMDB slot, so it is `tmdbConcluded`
   *  AND carries `tmdbTitleAliases` — the two properties the landing walk charged for. */
  private def concludedRow(title: String, cin: Cinema): MovieRecord =
    MovieRecord(tmdbId = Some(title.hashCode.abs),
      data = Map[Source, SourceData](
        (cin: Source) -> SourceData(title = Some(title), rawTitle = Some(title), releaseYear = Some(2026)),
        (Tmdb: Source) -> SourceData(title = Some(title), originalTitle = Some(s"$title Original"),
                                     englishTitle = Some(s"$title English"), releaseYear = Some(2026))))

  private def corpusRow(title: String, cin: Cinema): MovieRecord =
    MovieRecord(tmdbId = Some(title.hashCode.abs),
      data = Map[Source, SourceData]((cin: Source) -> SourceData(
        title = Some(title), rawTitle = Some(title), releaseYear = Some(2026))))

  private def scrapeOf(title: String): CinemaMovie =
    CinemaMovie(Movie(title = title, releaseYear = Some(2026)),
      cinema, posterUrl = None, filmUrl = None, synopsis = None,
      cast = Nil, director = Nil, showtimes = Nil)

  /** Sanitize calls made by ONE venue's scrape into a corpus of `corpusSize` other films. */
  private def scrapeCost(corpusSize: Int): Int = {
    val normalizer = new CountingNormalizer(TitleNormalizer.forCountry(Country.default).rules)
    val staging    = new InMemoryStagingRepository(normalizer = normalizer)
    val cache      = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = normalizer),
                                            staging = Some(staging), normalizer = normalizer)
    // A corpus of unrelated films, half of them holding a slot at ANOTHER venue.
    (1 to corpusSize).foreach { n =>
      val title = s"Unrelated Film $n"
      cache.put(CacheKey(title, Some(2026), normalizer),
        corpusRow(title, if (n % 2 == 0) elsewhere else cinema))
    }
    // Seeding is not what is being measured — only the scrape that follows it.
    normalizer.calls = 0
    cache.recordCinemaScrape(cinema, Seq(scrapeOf("The Scraped Film")))
    normalizer.calls
  }

  "recordCinemaScrape" should "cost the same against a large corpus as against a small one" in {
    val small = scrapeCost(20)
    val large = scrapeCost(200)
    withClue(s"20-film corpus: $small sanitize calls; 200-film corpus: $large — " +
      "a scrape that reads the whole corpus scales with it, and a tick over every venue then costs O(venues x corpus): ") {
      large shouldBe small
    }
  }

  /** The staging half of the same walk: the prior-slot lookup and the venue prune read
   *  this cinema's rows, and read them with `findAll()` — which against Mongo decodes
   *  every staged document in the country to keep a handful, growing with the very
   *  backlog the tick is filling. `findByAnchor` was added to fix exactly this for the
   *  reaper; the scrape path kept the full scan. */
  /**
   * The LANDING half of the same rule, and the half the two tests above cannot see.
   *
   * They scrape a NEWCOMER, which diverts to staging and returns before
   * `concludedKeyFor` is ever called — so the walk behind the landing path sat outside
   * every existing probe. It is the more expensive of the two: for each row it runs
   * `isBareFilmTitle` (a `sanitize` of the row's own title) and then a `sanitize` of
   * every TMDB alias the row carries, once per LANDED LISTING. A warm corpus is almost
   * entirely concluded rows with two or three aliases each, which is why the United
   * States leg paid 72ms a listing against Germany's 8.7ms on identical code — Germany's
   * local run is barely resolved, so it barely pays it, and CI's warm one does.
   *
   * `redirectToExistingVariant` walked the corpus a second time on the same path, for
   * every listing this one missed. It is gone with this, but it compares a precomputed
   * `CacheKey.normalized` rather than sanitizing, so no counter can see it — the
   * traversal it shares with this one is what this test stands for.
   */
  it should "cost the same to LAND a listing on a concluded row whatever else the corpus holds" in {
    def landCost(corpusSize: Int): Int = {
      val normalizer = new CountingNormalizer(TitleNormalizer.forCountry(Country.default).rules)
      val staging    = new InMemoryStagingRepository(normalizer = normalizer)
      val cache      = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = normalizer),
                                              staging = Some(staging), normalizer = normalizer)
      // A corpus of CONCLUDED rows carrying TMDB aliases — what a warm country actually
      // holds, and what the walk charged per landed listing. `corpusRow` alone is
      // already `tmdbConcluded` (it has a tmdbId); the aliases are what make each row
      // cost more than one `sanitize` to reject.
      (1 to corpusSize).foreach { n =>
        val title = s"Unrelated Film $n"
        cache.put(CacheKey(title, Some(2026), normalizer), concludedRow(title, elsewhere))
      }
      // The row the scrape must LAND on, so the method runs to the end rather than
      // diverting a newcomer into staging.
      val landing = "The Scraped Film"
      cache.put(CacheKey(landing, Some(2026), normalizer), concludedRow(landing, cinema))
      normalizer.calls = 0
      cache.recordCinemaScrape(cinema, Seq(scrapeOf(landing)))
      normalizer.calls
    }
    val small = landCost(20)
    val large = landCost(200)
    withClue(s"20-film corpus: $small sanitize calls; 200-film corpus: $large — " +
      "landing a listing that reads every concluded row's aliases is O(listings x corpus), " +
      "and a tick over every venue then pays it once per listing: ") {
      large shouldBe small
    }
  }

  it should "read only its own cinema's staging rows, never the whole backlog" in {
    val normalizer = SingleCountryNormalizer.titleNormalizer
    var fullScans  = 0
    val staging = new InMemoryStagingRepository(normalizer = normalizer) {
      override def findAll(): Seq[StagingRecord] = { fullScans += 1; super.findAll() }
    }
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = normalizer),
                                       staging = Some(staging), normalizer = normalizer)
    cache.recordCinemaScrape(cinema, Seq(scrapeOf("A Newcomer")))
    fullScans shouldBe 0
  }
}
