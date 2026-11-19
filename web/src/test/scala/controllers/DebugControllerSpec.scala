package controllers

import tools.SpecClock.given

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{CinemaCityWroclavia, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.Instant

/**
 * `/debug` is the dev-only global-corpus table. It used to be city-scoped
 * (`/:city/debug`) even though the data is the whole corpus regardless of city,
 * so it's now a top-level `GET /debug` taking no city parameter. It renders (200) in
 * Dev/Test and 404s in Prod via the same `devOnly` gate as `/debug/tune`.
 */
class DebugControllerSpec extends AnyFlatSpec with Matchers {

  private val records = Seq(
    ("Belle", Some(2021), MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Belle"))))),
    ("Incepcja", Some(2010), MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Incepcja"))))),
  )

  private def buildController(mode: Mode): TestDebugController.DebugPages =
    TestDebugController.build(records, mode)._1

  "GET /debug" should "render the whole corpus in dev mode" in {
    val result = buildController(Mode.Dev).debug().apply(FakeRequest(GET, "/debug"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    // Both films listed regardless of any city — the table is the global corpus.
    html should include("Belle")
    html should include("Incepcja")
    // Deep-links into a city the film actually plays in (Wrocław), never a
    // city slug from the URL (there isn't one anymore).
    html should include("""href="/wroclaw/movie/belle"""")
    // The old top-of-page "Unenriched"/"TMDB-unresolved" pill sections are gone.
    // `data-flag` was their distinctive marker.
    html should not include ("data-flag=")
    // The heavy per-source breakdown is NOT rendered inline anymore — rendering
    // every row's (cinema × day × showtime) up front built one giant HTML string
    // that OOM'd the view on the full corpus. It's fetched per-row on expand from
    // /debug/details, so the page-level dump carries none of it. `cacheKey` is a
    // `<dt>` unique to the debugDetails partial — its absence proves laziness.
    html should not include ("cacheKey")
    // Each row still carries the URL the JS fetches its details from on expand.
    html should include ("""data-details-url=""")
  }

  it should "404 in production" in {
    val result = buildController(Mode.Prod).debug().apply(FakeRequest(GET, "/debug"))
    status(result) shouldBe NOT_FOUND
  }

  // ── /debug/cadence ────────────────────────────────────────────────────────

  private val cadenceNow = Instant.parse("2026-06-27T10:00:00Z")
  // Two sources of the same film (tmdbId 1 = "Belle"): MC backed off to the 4-day
  // cap (stable, backoff level 6) with a recorded change; IMDb fresh at the 2h base.
  private val cadenceStats = Seq(
    "mc|tmdb:1"   -> models.RatingChangeStats(6, 7, 1, cadenceNow, cadenceNow,
                       lastChange = Some(models.RatingChange(cadenceNow, "80", "85")),
                       backoffLevel = 6),
    "imdb|tmdb:1" -> models.RatingChangeStats(0, 1, 0, cadenceNow, cadenceNow)
  )
  private val cadenceReader: services.cadence.RatingCadenceReader = new services.cadence.RatingCadenceReader {
    override def all() = cadenceStats
    override def forKeys(keys: Seq[String]) = cadenceStats.toMap.view.filterKeys(keys.contains).toMap
  }
  private val cadenceRecords = Seq(("Belle", Some(2021), MovieRecord(tmdbId = Some(1))))

  "GET /debug/cadence" should "group films by refresh interval (slowest first) with the title and change history" in {
    val ctrl   = TestDebugController.build(cadenceRecords, Mode.Dev, ratingCadenceReader = cadenceReader)._1
    val result = ctrl.cadence().apply(FakeRequest(GET, "/debug/cadence"))
    status(result) shouldBe OK
    val html = contentAsString(result)
    html should include ("Belle")            // title resolved from the dedup key's tmdbId
    html should include ("every 4d")         // MC stretched to the cap
    html should include ("every 2h")         // IMDb at the base
    // The detail is now columns in a table, not a hover tooltip.
    html should include ("<table")
    html should include ("next refresh")     // the new column
    html should include ("last change")      // change history is a column now
    html should include ("80 → 85")          // the MC row's last change, rendered as from → to
    html should include ("(2026-0")          // next-refresh absolute timestamp (within 4d of 06-27)
    // Slowest (most backed-off) group renders first.
    html.indexOf("every 4d") should be < html.indexOf("every 2h")
    // Groups are collapsible (<details>) and FOLDED by default (no `open`).
    html should include ("<details")
    html should not include ("<details open")
    // The Filmweb (fw) pill uses Filmweb's orange.
    html should include (".site-fw   { background: #ff8c00; }")
  }

  it should "404 in production" in {
    val result = TestDebugController.build(Seq.empty, Mode.Prod)._1.cadence().apply(FakeRequest(GET, "/debug/cadence"))
    status(result) shouldBe NOT_FOUND
  }

  // ── /debug/details: the heavy per-source breakdown, fetched lazily per row ───
  // The corpus dump renders only the light data rows (above); a row's full
  // per-source breakdown is fetched on expand from this endpoint, keyed by the
  // row's Mongo `_id`. Rendering them all inline OOM'd the view on the full corpus.
  "GET /debug/details" should "render one row's per-source breakdown in dev" in {
    val id     = services.movies.StoredMovieRecord.keyFor("Belle", Some(2021), titleNormalizer)
    val result = buildController(Mode.Dev).debugDetails(id).apply(FakeRequest(GET, s"/debug/details?id=$id"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    // `cacheKey` (a `<dt>` unique to debugDetails) + the cinema slot that fed the
    // row are present here — exactly what the page dump above omits.
    html should include("cacheKey")
    html should include("Belle")
    html should include(CinemaCityWroclavia.displayName)
  }

  // An unreadable log is not an empty one: rendered empty, it says "never attempted" about a
  // film whose log the page simply could not read.
  it should "answer 503, not an empty enrichment log, when the log cannot be read" in {
    val resolved = Seq(("Belle", Some(2021), MovieRecord(tmdbId = Some(1),
      data = Map(CinemaCityWroclavia -> SourceData(title = Some("Belle"))))))
    val unreadable = new services.attempts.EnrichmentAttemptReader {
      override def forKeys(keys: Seq[String]) = throw new java.io.IOException("attempt log unreachable")
    }
    val (controller, _) = TestDebugController.build(resolved, Mode.Dev,
      ratingCadenceReader = cadenceReader, attemptReader = unreadable)
    val id     = services.movies.StoredMovieRecord.keyFor("Belle", Some(2021), titleNormalizer)
    val result = controller.debugDetails(id).apply(FakeRequest(GET, s"/debug/details?id=$id"))
    status(result) shouldBe SERVICE_UNAVAILABLE
    contentAsString(result) should include ("attempt log unreachable")
  }

  // The enrichment log is the answer to "why does this film have no rating" —
  // it must distinguish a source that ERRORED from one that simply reported no
  // change, and show the backoff that decides when it will be retried.
  it should "render each rating source's last attempt, its error, and its backoff" in {
    val resolved = Seq(("Belle", Some(2021), MovieRecord(tmdbId = Some(1),
      data = Map(CinemaCityWroclavia -> SourceData(title = Some("Belle"))))))
    val attempts = new services.attempts.InMemoryEnrichmentAttemptStore
    attempts.record("imdb|tmdb:1", models.EnrichmentAttempt(
      cadenceNow, 120, models.AttemptOutcome.Changed("8.3")))
    attempts.record("rt|tmdb:1", models.EnrichmentAttempt(
      cadenceNow, 30, models.AttemptOutcome.Failed("IOException: connect timed out")))

    val (controller, _) = TestDebugController.build(resolved, Mode.Dev,
      ratingCadenceReader = cadenceReader, attemptReader = attempts)
    val id     = services.movies.StoredMovieRecord.keyFor("Belle", Some(2021), titleNormalizer)
    val result = controller.debugDetails(id).apply(FakeRequest(GET, s"/debug/details?id=$id"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    html should include("Enrichment attempts")
    html should include("8.3")                              // the IMDb change
    html should include("IOException: connect timed out")   // the RT error, verbatim
    html should include("attempt-failed")                   // and flagged as a failure
    html should include("L6")                               // MC's backoff level from the cadence
    html should include("never")                            // MC/Filmweb never attempted
    // Position is part of the contract: the enrichment state is what you open a
    // row FOR, and Odyseja's slot list ran 8,774 lines — enough to bury it
    // entirely when it was appended last. Pin the whole sequence, not just one
    // side of it: general details (posterUrl is the last of them) → enrichment →
    // first cinema slot.
    val posterAt = html.indexOf("posterUrl")
    val enrichAt = html.indexOf("Enrichment attempts")
    val cinemaAt = html.indexOf(CinemaCityWroclavia.displayName)
    info(s"posterUrl@$posterAt < enrichment@$enrichAt < first cinema@$cinemaAt")
    all(Seq(posterAt, enrichAt, cinemaAt)) should be >= 0
    posterAt should be < enrichAt
    enrichAt should be < cinemaAt
  }

  it should "explain an unresolved row instead of showing an empty table" in {
    val id     = services.movies.StoredMovieRecord.keyFor("Belle", Some(2021), titleNormalizer)
    val result = buildController(Mode.Dev).debugDetails(id).apply(FakeRequest(GET, s"/debug/details?id=$id"))
    contentAsString(result) should include("No tmdbId")
  }

  it should "404 an unknown id" in {
    val result = buildController(Mode.Dev).debugDetails("nope|1999").apply(FakeRequest(GET, "/debug/details?id=nope|1999"))
    status(result) shouldBe NOT_FOUND
  }

  it should "404 in production like the rest of /debug" in {
    val id     = services.movies.StoredMovieRecord.keyFor("Belle", Some(2021), titleNormalizer)
    val result = buildController(Mode.Prod).debugDetails(id).apply(FakeRequest(GET, s"/debug/details?id=$id"))
    status(result) shouldBe NOT_FOUND
  }
}
