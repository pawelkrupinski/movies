package services.enrichment

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import tools.GetOnlyHttpFetch

import java.net.URLDecoder
import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.duration._
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Tests for `OmdbBackfill` — the OMDb IMDb-id backfill. It fills a missing
 * `imdbId` (by title search), never a rating value, never a rating site's link
 * (`RottenTomatoesRatings` owns `rottenTomatoesUrl`), and never overrides an id a
 * canonical writer already supplied.
 *
 * OMDb HTTP is stubbed: a `?t=` request echoes the queried title back with a
 * canned imdbID (so the title-match guard passes); a `?i=` request would carry a
 * tomatoURL, which the backfill must not write.
 */
class OmdbBackfillSpec extends AnyFlatSpec with Matchers {

  private val RtUrl = "https://www.rottentomatoes.com/m/the_film"

  /** Echoes the `?t=` title back (guard passes) with a canned id; serves a
   *  tomatoURL for `?i=`, which must never reach the row. Key present so the calls fire. */
  private def omdbStub: OMDbClient = new OMDbClient(
    http = new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url.contains("?t=")) {
          val t = URLDecoder.decode(url.split("[?&]").find(_.startsWith("t=")).map(_.drop(2)).getOrElse(""), "UTF-8")
          s"""{"Title":"$t","imdbID":"tt0133093","Response":"True"}"""
        } else if (url.contains("?i="))
          s"""{"tomatoURL":"$RtUrl","Response":"True"}"""
        else """{"Response":"False","Error":"Movie not found!"}"""
    },
    apiKey = Some(settings.OmdbApiKey("test-key"))
  )

  private def cacheWith(record: MovieRecord) =
    new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Film", Some(2024), record)), normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
  private def keyOf(cache: CaffeineMovieCache) = cache.keyOf("Film", Some(2024))

  // ── golden path: recover the IMDb id, and only that ───────────────────────────

  "refreshOneSync" should "recover a missing imdbId by title and write no Rotten Tomatoes link" in {
    val cache = cacheWith(MovieRecord())
    new OmdbBackfill(cache, omdbStub, clock = _root_.tools.SpecClock.Pinned).refreshOneSync(keyOf(cache))

    val e = cache.get(keyOf(cache)).get
    e.imdbId            shouldBe Some("tt0133093")
    // The RT link is RottenTomatoesRatings' to find and verify: a second writer raced it.
    e.rottenTomatoesUrl shouldBe None
    // Nor any rating value — those stay for the canonical writers.
    e.imdbRating     shouldBe None
    e.rottenTomatoes shouldBe None
  }

  // ── never override an existing identifier ─────────────────────────────────────

  it should "make no OMDb call for a row that already has its imdbId, whatever its RT link" in {
    val cache = cacheWith(MovieRecord(imdbId = Some("tt7654321")))  // canonical id, no RT link
    val backfill = new OmdbBackfill(cache, new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("imdbId present — no call") },
      apiKey = Some(settings.OmdbApiKey("test-key"))
    ), clock = _root_.tools.SpecClock.Pinned)
    noException should be thrownBy backfill.refreshOneSync(keyOf(cache))

    val e = cache.get(keyOf(cache)).get
    e.imdbId            shouldBe Some("tt7654321")
    e.rottenTomatoesUrl shouldBe None
  }

  it should "recover imdbId via title search when only the rottenTomatoesUrl is already set" in {
    val cache = cacheWith(MovieRecord(rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/existing")))
    new OmdbBackfill(cache, omdbStub, clock = _root_.tools.SpecClock.Pinned).refreshOneSync(keyOf(cache))

    val e = cache.get(keyOf(cache)).get
    e.imdbId            shouldBe Some("tt0133093")                                  // recovered
    e.rottenTomatoesUrl shouldBe Some("https://www.rottentomatoes.com/m/existing")  // untouched
  }

  it should "make NO write when OMDb cannot supply the imdbId" in {
    val repository = new InMemoryMovieRepository(Seq(("Film", Some(2024), MovieRecord())), normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    repository.upserts.clear()
    // ?t= returns no match → nothing to write.
    val omdb = new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = """{"Response":"False","Error":"Movie not found!"}""" },
      apiKey = Some(settings.OmdbApiKey("test-key"))
    )
    new OmdbBackfill(cache, omdb, clock = _root_.tools.SpecClock.Pinned).refreshOneSync(keyOf(cache))
    repository.upserts shouldBe empty
  }

  // ── eligibility / gating ─────────────────────────────────────────────────────

  it should "be a no-op (no HTTP) when the row already has both imdbId and rottenTomatoesUrl" in {
    val cache = cacheWith(MovieRecord(imdbId = Some("tt0133093"), rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/existing")))
    val backfill = new OmdbBackfill(cache, new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("nothing missing — no call") },
      apiKey = Some(settings.OmdbApiKey("test-key"))
    ), clock = _root_.tools.SpecClock.Pinned)
    noException should be thrownBy backfill.refreshOneSync(keyOf(cache))
  }

  it should "be a no-op when the OMDb key is unset (feature off — no HTTP call)" in {
    val cache = cacheWith(MovieRecord())
    val keyless = new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("feature off — no call") },
      apiKey = None
    )
    new OmdbBackfill(cache, keyless, clock = _root_.tools.SpecClock.Pinned).refreshOneSync(keyOf(cache))

    val e = cache.get(keyOf(cache)).get
    e.imdbId shouldBe None
    e.rottenTomatoesUrl shouldBe None
  }

  it should "record no miss when OMDb cannot be read, so the next sweep asks again" in {
    val cache    = cacheWith(MovieRecord())
    val attempts = new InMemoryOmdbAttemptStore
    val failing  = new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new tools.HttpStatusException(401, "GET", url, None) },
      apiKey = Some(settings.OmdbApiKey("test-key"))
    )
    val result = new OmdbBackfill(cache, failing, attempts, clock = _root_.tools.SpecClock.Pinned).refreshAll()
    result.failed shouldBe Some(1)
    attempts.all() shouldBe empty
    cache.get(keyOf(cache)).get.imdbId shouldBe None
  }

  it should "propagate an OMDb failure to the task (its retry sees it) and leave the row untouched" in {
    val cache = cacheWith(MovieRecord())
    val failing = new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new tools.HttpStatusException(503, "GET", url, None) },
      apiKey = Some(settings.OmdbApiKey("test-key"))
    )
    a[tools.HttpStatusException] should be thrownBy new OmdbBackfill(cache, failing, clock = _root_.tools.SpecClock.Pinned).refreshOneSync(keyOf(cache))
    cache.get(keyOf(cache)).get.imdbId shouldBe None
  }

  // ── full-corpus walk ─────────────────────────────────────────────────────────

  "refreshAll" should "recover the imdbId for every row missing one and leave every RT link alone" in {
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, MovieRecord()),                                                          // imdbId missing
      ("B", None, MovieRecord(imdbId = Some("tt0002"))),                                   // has its id → skip
      ("C", None, MovieRecord(imdbId = Some("tt0003"), rottenTomatoesUrl = Some(RtUrl)))   // fully identified → skip
    ), normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    new OmdbBackfill(cache, omdbStub, clock = _root_.tools.SpecClock.Pinned).refreshAll()

    cache.get(cache.keyOf("A", None)).get.imdbId            shouldBe Some("tt0133093") // recovered
    cache.get(cache.keyOf("A", None)).get.rottenTomatoesUrl shouldBe None
    cache.get(cache.keyOf("B", None)).get.imdbId            shouldBe Some("tt0002")    // untouched
    cache.get(cache.keyOf("B", None)).get.rottenTomatoesUrl shouldBe None              // not OMDb's to write
    cache.get(cache.keyOf("C", None)).get.rottenTomatoesUrl shouldBe Some(RtUrl)       // unchanged
  }

  it should "read the backoff store ONCE for the whole sweep, not a blocking read per candidate row" in {
    // The credit-drain bug: refreshAll did a per-row Mongo `get` (inBackoff) for
    // every row missing an id, corpus-wide, every sweep. It must now do ONE
    // batched `all()` read instead.
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, MovieRecord()),                                  // imdbId missing
      ("B", None, MovieRecord()),                                  // imdbId missing
      ("C", None, MovieRecord()),                                  // imdbId missing
      ("D", None, MovieRecord(imdbId = Some("tt9")))               // has its id
    ), normalizer = titleNormalizer)
    val cache    = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val attempts = new CountingOmdbAttemptStore
    new OmdbBackfill(cache, omdbStub, attempts, clock = _root_.tools.SpecClock.Pinned).refreshAll()

    attempts.allCalls shouldBe 1 // one batched read for the whole sweep
    attempts.getCalls shouldBe 0 // never a per-row blocking read (the drain)
  }

  /** Counts reads so a per-row `get` regression in the sweep is caught. */
  private class CountingOmdbAttemptStore extends OmdbAttemptStore {
    var getCalls = 0
    var allCalls = 0
    private val inner = new InMemoryOmdbAttemptStore
    def get(filmKey: String): Option[OmdbAttempt] = { getCalls += 1; inner.get(filmKey) }
    def all(): Map[String, OmdbAttempt] = { allCalls += 1; inner.all() }
    def record(filmKey: String, level: Int, at: Instant): Unit = inner.record(filmKey, level, at)
  }

  // ── backoff ──────────────────────────────────────────────────────────────────

  private val T0 = Instant.parse("2026-06-01T00:00:00Z")
  // The film key OmdbBackfill stores under — derived the same way (cleanTitle|year).
  private def filmKey(cache: CaffeineMovieCache): String = {
    val k = keyOf(cache); s"${k.cleanTitle}|${k.year.map(_.toString).getOrElse("")}"
  }

  "backoff" should "skip a film still within its backoff window — no HTTP call" in {
    val cache = cacheWith(MovieRecord())
    val attempts = new InMemoryOmdbAttemptStore
    attempts.record(filmKey(cache), 1, T0) // missed at T0, level 1 → 2-day window
    var httpCalls = 0
    val omdb = new OMDbClient(new GetOnlyHttpFetch { def get(url: String): String = { httpCalls += 1; """{"Response":"False","Error":"Movie not found!"}""" } }, apiKey = Some(settings.OmdbApiKey("k")))
    val clock = Clock.fixed(T0.plusMillis(1.hour.toMillis), ZoneOffset.UTC) // 1h later — inside the 2d window

    new OmdbBackfill(cache, omdb, attempts, clock).refreshOneSync(keyOf(cache))
    httpCalls shouldBe 0
    cache.get(keyOf(cache)).get.imdbId shouldBe None
  }

  it should "record a miss (level 1) when OMDb resolves nothing" in {
    val cache = cacheWith(MovieRecord())
    val attempts = new InMemoryOmdbAttemptStore
    val omdb = new OMDbClient(new GetOnlyHttpFetch { def get(url: String): String = """{"Response":"False","Error":"Movie not found!"}""" }, apiKey = Some(settings.OmdbApiKey("k")))

    new OmdbBackfill(cache, omdb, attempts, Clock.fixed(T0, ZoneOffset.UTC)).refreshOneSync(keyOf(cache))
    attempts.get(filmKey(cache)).map(_.level) shouldBe Some(1)
  }

  it should "re-probe once the backoff window has elapsed" in {
    val cache = cacheWith(MovieRecord())
    val attempts = new InMemoryOmdbAttemptStore
    attempts.record(filmKey(cache), 1, T0) // 2-day window from T0
    val clock = Clock.fixed(T0.plusMillis(3.days.toMillis), ZoneOffset.UTC) // past the window

    new OmdbBackfill(cache, omdbStub, attempts, clock).refreshOneSync(keyOf(cache))
    cache.get(keyOf(cache)).get.imdbId shouldBe Some("tt0133093") // re-probed + recovered
  }

  "OmdbBackfill.backoffWindow" should "double per consecutive miss and cap at 30 days" in {
    OmdbBackfill.backoffWindow(0) shouldBe Duration.Zero
    OmdbBackfill.backoffWindow(1) shouldBe 2.days
    OmdbBackfill.backoffWindow(2) shouldBe 4.days
    OmdbBackfill.backoffWindow(4) shouldBe 16.days
    OmdbBackfill.backoffWindow(5) shouldBe 30.days // 32d capped
    OmdbBackfill.backoffWindow(9) shouldBe 30.days
  }
}
