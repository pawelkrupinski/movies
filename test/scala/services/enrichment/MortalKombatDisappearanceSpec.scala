package services.enrichment

import clients.TmdbClient
import clients.tools.FakeHttpFetch
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{CinemaCityClient, HeliosClient, MultikinoClient}
import services.events.{EventBus, MovieAdded}
import tools.HttpFetch

import scala.collection.mutable

/**
 * Reproduction of the "Mortal Kombat II disappears" report.
 *
 * Three cinemas serve the same film under different `(cleanTitle, year)`
 * tuples after their parsers run:
 *
 *   Multikino    → ("Mortal Kombat 2",  None)         — MultikinoParser
 *                  drops the Polish theatrical date on purpose.
 *   CinemaCity   → ("Mortal Kombat II", None)         — releaseYear is a
 *                  STRING in the payload ("2026"); `asOpt[Int]` returns
 *                  None, so the parser effectively drops the year too.
 *   Helios       → ("Mortal Kombat II", Some(2025))   — yearOfProduction
 *                  is the production year (2025), not the 2026 release.
 *
 * Multikino + CinemaCity collapse to the same CacheKey ("mortalkombatii",
 * None) — `CacheKey.equals` normalises the cleanTitle so "Mortal Kombat 2"
 * and "Mortal Kombat II" share a row. Helios lands on a separate row
 * because the year differs. The TMDB stage resolves the year=2025 row via
 * the sister-row alias path; the log line in production is:
 *
 *   Sister-row match: Mortal Kombat II (2025) → tmdbId=931285 imdbId=tt17490712 via aliases=mortalkombatii
 *
 * The bug this spec pins down: `runTmdbStageSync` builds a fresh
 * `MovieRecord(...)` from scratch with the defaulted `cinemaShowings =
 * Map.empty` and writes it via `cache.put`, which preserves `cinemaTitles`
 * but NOT `cinemaShowings`. That wipes whatever cinema slot
 * `recordCinemaScrape` just landed on the row. The row lingers in
 * `snapshot()` with zero showtimes, and `MovieController.toSchedules`
 * filters it out — the film disappears from the home-page list.
 */
class MortalKombatDisappearanceSpec extends AnyFlatSpec with Matchers {

  // ── Stubs ──────────────────────────────────────────────────────────────────

  private class StubFetch(routes: Map[String, String]) extends HttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
  }

  // TMDB returns Mortal Kombat II (tmdbId 931285) for any title search.
  // /external_ids → tt17490712.
  private val Mk2Search =
    """{"results":[{"id":931285,"title":"Mortal Kombat II","original_title":"Mortal Kombat II",""" +
    """"release_date":"2026-05-06","popularity":223.66}]}"""
  private val Mk2ExternalIds = """{"id":931285,"imdb_id":"tt17490712"}"""

  private def tmdbStub() = new TmdbClient(
    http = new StubFetch(Map(
      "/search/movie" -> Mk2Search,
      "/external_ids" -> Mk2ExternalIds
    )),
    apiKey = Some("stub")
  )

  // Mirrors `MovieRepo`'s `docId = normalize(title)|year` keying: two
  // upserts of the same film under different raw titles ("Mortal Kombat 2"
  // vs "Mortal Kombat II") collapse to one persisted row, just like Mongo.
  private class FakeRepo extends MovieRepo {
    private def docId(t: String, y: Option[Int]): String =
      s"${MovieService.normalize(t)}|${y.map(_.toString).getOrElse("")}"
    private val store = mutable.LinkedHashMap.empty[String, (String, Option[Int], MovieRecord)]
    override def enabled: Boolean = true
    override def findAll(): Seq[(String, Option[Int], MovieRecord)] = store.values.toSeq
    override def upsert(t: String, y: Option[Int], e: MovieRecord): Unit = {
      store.put(docId(t, y), (t, y, e)); ()
    }
    override def delete(t: String, y: Option[Int]): Unit = { store.remove(docId(t, y)); () }
  }

  // ── Step 1: clients DO fetch the film from their fixtures ─────────────────
  //
  // Each cinema's fixture is the same recorded payload that drives the
  // production scrape — replaying it through the parsers proves the
  // "disappears" report isn't an upstream-missing case.

  private val multikinoMk =
    new MultikinoClient(new FakeHttpFetch("multikino")).fetch()
      .find(_.movie.title == "Mortal Kombat 2").get
  private val cinemaCityMk =
    new CinemaCityClient(new FakeHttpFetch("cinema-city-plaza"))
      .fetch("1078", CinemaCityPoznanPlaza)
      .filter(_.movie.title == "Mortal Kombat II")
      .head
  private val heliosMk =
    new HeliosClient(new FakeHttpFetch("helios/rest-enrichment")).fetch()
      .filter(_.movie.title == "Mortal Kombat II")
      .head

  "cinema clients" should "all fetch the film from their fixtures (each cinema reports it differently)" in {
    multikinoMk.movie.title       shouldBe "Mortal Kombat 2"
    multikinoMk.movie.releaseYear shouldBe None
    multikinoMk.showtimes         should not be empty

    cinemaCityMk.movie.title       shouldBe "Mortal Kombat II"
    // CinemaCity's payload encodes "releaseYear":"2026" as a string and the
    // parser uses `asOpt[Int]`, so the year drops to None.
    cinemaCityMk.movie.releaseYear shouldBe None
    cinemaCityMk.showtimes         should not be empty

    heliosMk.movie.title           shouldBe "Mortal Kombat II"
    heliosMk.movie.releaseYear     shouldBe Some(2025)
    heliosMk.showtimes             should not be empty
  }

  // ── Step 2: the TMDB stage wipes the cinema slot ──────────────────────────
  //
  // Smallest reproduction. One cinema (Multikino), one scrape, one
  // synchronous TMDB resolution — the slot we just wrote is gone.

  it should "preserve Multikino's slot when the TMDB stage resolves the row" in {
    val cache = new MovieCache(new FakeRepo)
    val svc   = new MovieService(cache, new EventBus, tmdbStub())

    cache.recordCinemaScrape(Multikino, Seq(multikinoMk))

    val key = cache.keyOf("Mortal Kombat 2", None)
    cache.get(key).get.cinemaShowings.keySet shouldBe Set(Multikino)

    // Same code path the MovieAdded-driven async wrapper invokes — bypasses
    // the worker pool for a deterministic single-thread reproduction.
    svc.reEnrichSync("Mortal Kombat 2", None)

    val row = cache.get(key).get
    row.tmdbId shouldBe Some(931285)
    row.imdbId shouldBe Some("tt17490712")
    // Currently FAILS: runTmdbStageSync constructs a fresh MovieRecord(...)
    // with the default empty cinemaShowings and writes it through
    // cache.put, which only re-folds cinemaTitles — the Multikino slot is
    // wiped. The view layer (MovieController.toSchedules) then drops this
    // row because allShowtimes is empty.
    row.cinemaShowings.keySet shouldBe Set(Multikino)
  }

  // ── Step 3: end-to-end — three cinemas + sister-row resolution + merge ────
  //
  // Reproduces the user-visible disappearance exactly. Each cinema's
  // recordCinemaScrape lands a slot, the subsequent TMDB stage wipes it,
  // and IdentityMerger collapses the empty rows into one empty row.

  it should "carry every cinema's showings through the full pipeline (all three slots survive)" in {
    val cache  = new MovieCache(new FakeRepo)
    val bus    = new EventBus
    val svc    = new MovieService(cache, bus, tmdbStub())
    val merger = new IdentityMerger(cache)

    // Multikino tick. Lands at ("mortalkombatii", None).
    cache.recordCinemaScrape(Multikino, Seq(multikinoMk))
    svc.reEnrichSync("Mortal Kombat 2", None)

    // CinemaCity tick. CacheKey normalises to the SAME ("mortalkombatii",
    // None) as Multikino, so the slot updates the same row.
    cache.recordCinemaScrape(CinemaCityPoznanPlaza, Seq(cinemaCityMk))
    svc.reEnrichSync("Mortal Kombat II", None)

    // Helios tick — year=2025 lands on its own row. Resolution falls back
    // to the sister-row alias path (matching the production log).
    cache.recordCinemaScrape(Helios, Seq(heliosMk))
    svc.reEnrichSync("Mortal Kombat II", Some(2025))

    // Collapse the rows. mergeFor is the sync entry point that matches
    // IdentityMerger's event-driven contract one trigger at a time.
    merger.mergeFor("Mortal Kombat 2",  None)
    merger.mergeFor("Mortal Kombat II", Some(2025))

    val snapshot = cache.snapshot()
    snapshot.size shouldBe 1
    val (_, _, survivor) = snapshot.head
    survivor.tmdbId shouldBe Some(931285)
    survivor.imdbId shouldBe Some("tt17490712")
    // Currently FAILS: each TMDB stage wiped its row's cinemaShowings
    // (see Step 2), so when the merger unions the empty maps the survivor
    // carries no slots. With zero showings, MovieController.toSchedules
    // drops the film from the listing → "Mortal Kombat II disappears".
    survivor.cinemaShowings.keySet should contain allOf (Multikino, CinemaCityPoznanPlaza, Helios)
    survivor.cinemaShowings.values.flatMap(_.showtimes) should not be empty

    merger.stop()
  }

  // ── Step 4: identity merge — exactly one entry, in cache and in repo ──────
  //
  // The three cinema scrapes go through two distinct CacheKeys
  // (("mortalkombatii", None) for Multikino+CinemaCity, and
  // ("mortalkombatii", Some(2025)) for Helios). `IdentityMerger` joins
  // siblings on shared tmdbId/imdbId, so the final state must carry one row
  // per film — anything more shows up as a duplicate card on the home page.

  it should "leave exactly one Mortal Kombat II row in cache and in the persisted repo" in {
    val repo   = new FakeRepo
    val cache  = new MovieCache(repo)
    val bus    = new EventBus
    val svc    = new MovieService(cache, bus, tmdbStub())
    val merger = new IdentityMerger(cache)

    cache.recordCinemaScrape(Multikino, Seq(multikinoMk))
    svc.reEnrichSync("Mortal Kombat 2", None)
    cache.recordCinemaScrape(CinemaCityPoznanPlaza, Seq(cinemaCityMk))
    svc.reEnrichSync("Mortal Kombat II", None)
    cache.recordCinemaScrape(Helios, Seq(heliosMk))
    svc.reEnrichSync("Mortal Kombat II", Some(2025))

    // Two distinct rows before the merger runs — same tmdbId, different year.
    val preMerge = cache.snapshot().filter { case (_, _, e) => e.tmdbId.contains(931285) }
    preMerge.size shouldBe 2

    merger.mergeFor("Mortal Kombat 2",  None)
    merger.mergeFor("Mortal Kombat II", Some(2025))

    // Match by tmdbId/imdbId, not title — that's the merger's identity rule
    // and the right anchor for "same film, possibly under a different key".
    def isMk2(e: MovieRecord): Boolean =
      e.tmdbId.contains(931285) || e.imdbId.contains("tt17490712")

    // Cache: one row after the merge.
    cache.snapshot().count { case (_, _, e) => isMk2(e) } shouldBe 1

    // Repo (Mongo write-through): one persisted row. The merger deletes the
    // loser keys via `cache.invalidate`, which propagates to `repo.delete`,
    // so a successful merge must leave no orphan persisted row behind.
    repo.findAll().count { case (_, _, e) => isMk2(e) } shouldBe 1

    // Screen: `displayTitle` is the card label; a single snapshot row means
    // a single rendered entry, and the picked label folds both raw titles.
    val (_, _, survivor) = cache.snapshot().find { case (_, _, e) => isMk2(e) }.get
    survivor.displayTitle              shouldBe "Mortal Kombat II"
    survivor.cinemaTitles              should contain allOf ("Mortal Kombat 2", "Mortal Kombat II")
    survivor.cinemaShowings.keySet     shouldBe Set(Multikino, CinemaCityPoznanPlaza, Helios)

    merger.stop()
  }

  // ── Step 5: every scrape order leaves exactly one visible row ─────────────
  //
  // IdentityMerger reconciles duplicate rows asynchronously on its own
  // worker pool — there's a window between TMDB-stage completion and merger
  // execution during which a user request would see multiple cache rows
  // for the same film. The fix at the scrape layer is to consolidate same-
  // film variants onto a single row eagerly, so any extra rows the merger
  // is yet to clean up carry no cinema slots and `toSchedules` filters
  // them out. The invariant: at most one row in the snapshot has
  // `cinemaShowings.nonEmpty` for any given film, regardless of which
  // cinema scrapes first.

  private case class Scrape(cinema: Cinema, title: String, year: Option[Int], cm: CinemaMovie)
  private def scrapes = Seq(
    Scrape(Multikino,             "Mortal Kombat 2",  None,        multikinoMk),
    Scrape(CinemaCityPoznanPlaza, "Mortal Kombat II", None,        cinemaCityMk),
    Scrape(Helios,                "Mortal Kombat II", Some(2025),  heliosMk)
  )

  for (ordering <- scrapes.permutations.toList) {
    val label = ordering.map(_.cinema.getClass.getSimpleName.stripSuffix("$")).mkString(" → ")
    s"scrape order $label" should "leave exactly one visible Mortal Kombat II row (no merger run)" in {
      val cache = new MovieCache(new FakeRepo)
      val bus   = new EventBus
      val svc   = new MovieService(cache, bus, tmdbStub())

      // First scrape resolves the row synchronously so subsequent cinemas
      // can find a sibling with a tmdbId — that's what
      // `hasResolvedSiblingByTitle` needs to short-circuit the TMDB stage.
      val first = ordering.head
      cache.recordCinemaScrape(first.cinema, Seq(first.cm))
      svc.reEnrichSync(first.title, first.year)

      // Subsequent cinemas follow the production flow: recordCinemaScrape
      // first (redirect folds the slot into the existing sibling row),
      // then publish the MovieAdded `ShowtimeCache` would publish. The
      // bus listener's `scheduleTmdbStage` must short-circuit via
      // `hasResolvedSiblingByTitle` (normalize-match) so no phantom row
      // is created at the raw (title, year) key.
      bus.subscribe(svc.onMovieAdded)
      for (s <- ordering.tail) {
        cache.recordCinemaScrape(s.cinema, Seq(s.cm))
        bus.publish(MovieAdded(s.title, s.year, s.cm.movie.originalTitle, s.cm.director))
      }

      def isMk2(e: MovieRecord): Boolean =
        e.tmdbId.contains(931285) || e.imdbId.contains("tt17490712")

      val visibleRows = cache.snapshot().filter { case (_, _, e) =>
        isMk2(e) && e.cinemaShowings.nonEmpty
      }
      visibleRows.size shouldBe 1
      val (_, _, visible) = visibleRows.head
      visible.cinemaShowings.keySet shouldBe Set(Multikino, CinemaCityPoznanPlaza, Helios)
    }
  }
}
