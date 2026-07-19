package controllers

import models.{CinemaCityWroclavia, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.staging.StagingRecord
import services.tasks.{EnrichTaskKeys, InMemoryTaskQueue, TaskState, TaskSummary, TaskType}

import java.time.Instant

/**
 * `/debug` is the dev-only global-corpus table. It used to be city-scoped
 * (`/:city/debug`) even though the data is the whole corpus regardless of city,
 * so it's now a top-level `GET /debug` taking no city parameter. It renders (200) in
 * Dev/Test and 404s in Prod via the same `devOnly` gate as `/debug/tune`.
 */
class MovieControllerDebugSpec extends AnyFlatSpec with Matchers {

  private val records = Seq(
    ("Belle", Some(2021), MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Belle"))))),
    ("Incepcja", Some(2010), MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Incepcja"))))),
  )

  private def buildController(mode: Mode): MovieController =
    TestMovieController.build(records, mode)._1

  "GET /debug" should "render the whole corpus in dev mode" in {
    val result = buildController(Mode.Dev).debug().apply(FakeRequest(GET, "/debug"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    // Both films listed regardless of any city — the table is the global corpus.
    html should include("Belle")
    html should include("Incepcja")
    // Deep-links into a city the film actually plays in (Wrocław), never a
    // city slug from the URL (there isn't one anymore).
    html should include("""href="/wroclaw/film?title=Belle"""")
    // The old top-of-page "Unenriched"/"TMDB-unresolved" pill sections are gone —
    // the staging table is now the only pending-work view. `data-flag` was their
    // distinctive marker.
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
    "mc|tmdb:1"   -> services.cadence.RatingChangeStats(6, 7, 1, cadenceNow, cadenceNow,
                       lastChange = Some(services.cadence.RatingChange(cadenceNow, "80", "85")),
                       backoffLevel = 6),
    "imdb|tmdb:1" -> services.cadence.RatingChangeStats(0, 1, 0, cadenceNow, cadenceNow)
  )
  private val cadenceReader: services.cadence.RatingCadenceReader = new services.cadence.RatingCadenceReader {
    override def all() = cadenceStats
    override def forKeys(keys: Seq[String]) = cadenceStats.toMap.view.filterKeys(keys.contains).toMap
  }
  private val cadenceRecords = Seq(("Belle", Some(2021), MovieRecord(tmdbId = Some(1))))

  "GET /debug/cadence" should "group films by refresh interval (slowest first) with the title and change history" in {
    val ctrl   = TestMovieController.build(cadenceRecords, Mode.Dev, ratingCadenceReader = cadenceReader)._1
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
    val result = TestMovieController.build(Seq.empty, Mode.Prod)._1.cadence().apply(FakeRequest(GET, "/debug/cadence"))
    status(result) shouldBe NOT_FOUND
  }

  // `/debug` reads two full-collection Mongo scans (`movies` + `pending_movies`).
  // Being dev-only it is always served over the slow local→prod Mongo tunnel,
  // where each cursor is ~6 s, so doing them one-after-another doubled the page's
  // cold-load latency. Prove the two reads overlap with a 2-party rendezvous:
  // each repository's findAll() blocks at the barrier until the other arrives.
  // Concurrent reads both reach it and trip it; a sequential implementation
  // leaves the first read waiting for a second that hasn't been dispatched yet,
  // so it times out and the flag stays false.
  it should "read the corpus and staging collections concurrently, not one after the other" in {
    val barrier = new java.util.concurrent.CyclicBarrier(2)
    @volatile var bothScansOverlapped = false
    def rendezvous(): Unit =
      try {
        barrier.await(3, java.util.concurrent.TimeUnit.SECONDS)
        bothScansOverlapped = true
      } catch { case _: Throwable => () } // timeout / broken barrier ⇒ ran sequentially

    val movieRepo = new services.movies.InMemoryMovieRepository(records) {
      override def findAll(): Seq[services.movies.StoredMovieRecord] = { rendezvous(); super.findAll() }
    }
    val stagingRepo = new services.staging.StagingRepository {
      def enabled: Boolean = true
      def findAll(): Seq[services.staging.StagingRecord] = { rendezvous(); Seq.empty }
      def upsert(cinema: models.Source, title: String, year: Option[Int], record: MovieRecord): Unit = ()
      def delete(cinema: models.Source, title: String, year: Option[Int]): Unit = ()
    }

    val ctrl = TestMovieController.build(records, Mode.Dev,
      movieRepository = Some(movieRepo), stagingRepository = stagingRepo)._1

    status(ctrl.debug().apply(FakeRequest(GET, "/debug"))) shouldBe OK
    bothScansOverlapped shouldBe true
  }

  // The staging table is folded by film client-side: the server emits the column
  // headers, the hidden `#staging-src` source tbody (one data-only row per cinema)
  // and the empty `#staging-folded` tbody the JS fills. The source row carries the
  // `data-anchor` the queue badge matches on + the per-cinema fold inputs.
  "GET /debug" should "render the staging fold scaffolding + per-cinema source row" in {
    val staging = new services.staging.InMemoryStagingRepository(Seq(
      (CinemaCityWroclavia, "Newcomer", Some(2099),
        MovieRecord(detailPending = true, data = Map(CinemaCityWroclavia -> SourceData(title = Some("Newcomer")))))))
    val html = contentAsString(
      TestMovieController.build(records, Mode.Dev, stagingRepository = staging)._1
        .debug().apply(FakeRequest(GET, "/debug")))

    html should include ("<th>Cinemas</th>")
    html should include ("<th>Queue #</th>")
    html should include ("""id="staging-src"""")
    html should include ("""id="staging-folded"""")

    // The anchor = sanitize(title) the `staging-*` dedup keys (and so the queue
    // badge) match on, plus the per-cinema fold inputs, ride along on the source row.
    html should include ("""data-anchor="newcomer"""")
    html should include (s"""data-cinema="${CinemaCityWroclavia.displayName}"""")
    html should include ("""data-detail-done="false"""") // detail not yet fetched
  }

  // ── /debug/details: the heavy per-source breakdown, fetched lazily per row ───
  // The corpus dump renders only the light data rows (above); a row's full
  // per-source breakdown is fetched on expand from this endpoint, keyed by the
  // row's Mongo `_id`. Rendering them all inline OOM'd the view on the full corpus.
  "GET /debug/details" should "render one row's per-source breakdown in dev" in {
    val id     = services.movies.StoredMovieRecord.idFor("Belle", Some(2021))
    val result = buildController(Mode.Dev).debugDetails(id).apply(FakeRequest(GET, s"/debug/details?id=$id"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    // `cacheKey` (a `<dt>` unique to debugDetails) + the cinema slot that fed the
    // row are present here — exactly what the page dump above omits.
    html should include("cacheKey")
    html should include("Belle")
    html should include(CinemaCityWroclavia.displayName)
  }

  // The enrichment log is the answer to "why does this film have no rating" —
  // it must distinguish a source that ERRORED from one that simply reported no
  // change, and show the backoff that decides when it will be retried.
  it should "render each rating source's last attempt, its error, and its backoff" in {
    val resolved = Seq(("Belle", Some(2021), MovieRecord(tmdbId = Some(1),
      data = Map(CinemaCityWroclavia -> SourceData(title = Some("Belle"))))))
    val attempts = new services.attempts.InMemoryEnrichmentAttemptStore
    attempts.record("imdb|tmdb:1", services.attempts.EnrichmentAttempt(
      cadenceNow, 120, services.attempts.AttemptOutcome.Changed("8.3")))
    attempts.record("rt|tmdb:1", services.attempts.EnrichmentAttempt(
      cadenceNow, 30, services.attempts.AttemptOutcome.Failed("IOException: connect timed out")))

    val (controller, _) = TestMovieController.build(resolved, Mode.Dev,
      ratingCadenceReader = cadenceReader, attemptReader = attempts)
    val id     = services.movies.StoredMovieRecord.idFor("Belle", Some(2021))
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
    val id     = services.movies.StoredMovieRecord.idFor("Belle", Some(2021))
    val result = buildController(Mode.Dev).debugDetails(id).apply(FakeRequest(GET, s"/debug/details?id=$id"))
    contentAsString(result) should include("No tmdbId")
  }

  it should "404 an unknown id" in {
    val result = buildController(Mode.Dev).debugDetails("nope|1999").apply(FakeRequest(GET, "/debug/details?id=nope|1999"))
    status(result) shouldBe NOT_FOUND
  }

  it should "404 in production like the rest of /debug" in {
    val id     = services.movies.StoredMovieRecord.idFor("Belle", Some(2021))
    val result = buildController(Mode.Prod).debugDetails(id).apply(FakeRequest(GET, s"/debug/details?id=$id"))
    status(result) shouldBe NOT_FOUND
  }

  // ── ordering staging rows by their queue place ──────────────────────────────
  // The page renders only the first `StagingRowLimit` rows, so the most imminent
  // ones must sort to the top server-side: running first, then by waiting place.
  private def staged(title: String): StagingRecord =
    StagingRecord(CinemaCityWroclavia, title, Some(2026),
      MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some(title)))))
  private def task(taskType: String, dedupKey: String, state: String): TaskSummary =
    TaskSummary(dedupKey, taskType, dedupKey, state, Instant.EPOCH, 0, None, None, None)

  "orderStagingByQueue" should "sort running first, then by waiting place, then no-task last" in {
    // anchors: sanitize drops the space + lowercases → "runningfilm" etc.
    val rows = Seq("No Task", "Running Film", "Waiting Five", "Waiting Two").map(staged)
    val active = Seq( // oldest-first, as TaskQueue.monitor returns
      task("StagingDetail",        "staging-detail|runningfilm|cc", TaskState.WorkedOn), // ▶ running → 0
      task("StagingResolveTmdb",   "staging-tmdb|waitingtwo",       TaskState.Waiting),  // place 1
      task("ImdbRating",           "imdb-rating|x",                 TaskState.Waiting),  // place 2 (non-staging, bumps counter)
      task("StagingResolveImdbId", "staging-imdb|waitingfive",      TaskState.Waiting),  // place 3
    )
    MovieController.orderStagingByQueue(rows, active).map(_.title) shouldBe
      Seq("Running Film", "Waiting Two", "Waiting Five", "No Task")
  }

  it should "keep the incoming order for equal-rank (e.g. no-task) rows — a stable sort" in {
    val rows = Seq("Zebra", "Alpha").map(staged) // both have no active task → equal rank
    MovieController.orderStagingByQueue(rows, Seq.empty).map(_.title) shouldBe Seq("Zebra", "Alpha")
  }

  // ── /debug/queue snapshot the staging columns poll for queue places ─────────
  "GET /debug/queue" should "return the active tasks oldest-first in dev" in {
    val q  = new InMemoryTaskQueue
    val t0 = java.time.Instant.parse("2026-06-13T10:00:00Z")
    q.enqueue(TaskType.EnrichDetails, "detail|cc|Belle|2021", submittedAt = t0)
    q.enqueue(TaskType.ResolveTmdb, EnrichTaskKeys.resolveTmdbDedup("Incepcja", Some(2010)),
      submittedAt = t0.plusSeconds(1))
    val ctrl = TestMovieController.build(records, Mode.Dev, taskQueue = q)._1

    val result = ctrl.debugQueue().apply(FakeRequest(GET, "/debug/queue"))
    status(result) shouldBe OK
    val active = (Json.parse(contentAsString(result)) \ "active").as[Seq[play.api.libs.json.JsValue]]
    active.map(j => (j \ "taskType").as[String]) shouldBe Seq("EnrichDetails", "ResolveTmdb")
    active.map(j => (j \ "dedupKey").as[String]) shouldBe
      Seq("detail|cc|Belle|2021", EnrichTaskKeys.resolveTmdbDedup("Incepcja", Some(2010)))
    active.foreach(j => (j \ "state").as[String] shouldBe "waiting")
  }

  it should "404 in production like the rest of /debug" in {
    val ctrl = TestMovieController.build(records, Mode.Prod, taskQueue = new InMemoryTaskQueue)._1
    status(ctrl.debugQueue().apply(FakeRequest(GET, "/debug/queue"))) shouldBe NOT_FOUND
  }

  "GET /debug/readmodel" should "dump the warm read cache in dev mode" in {
    val result = buildController(Mode.Dev).debugReadModel().apply(FakeRequest(GET, "/debug/readmodel"))

    status(result) shouldBe OK
    val html = contentAsString(result)
    html should include("Read cache")
    // Reflects what the web serves from — the read cache's resolved movies.
    html should include("Belle")
    html should include("Incepcja")
  }

  // The read-cache dump surfaces a per-film "Cinemas" count (distinct venues
  // screening the film) AND, on expand, every field the read model holds for a
  // film + every field its per-cinema screenings hold — so a dev can see exactly
  // what the web serves without cross-referencing Mongo.
  it should "show a Cinemas column and every resolved movie/screening field on expand" in {
    import java.time.LocalDateTime
    import models.{HeliosMagnolia, Showtime}

    // One film, full metadata, screened by TWO distinct cinemas — so the Cinemas
    // count is 2 and both the movie-level and cinema-level fields are present.
    val rich = MovieRecord(data = Map(
      CinemaCityWroclavia -> SourceData(
        title          = Some("Belle"),
        originalTitle  = Some("Ryu to Sobakasu no Hime"),
        synopsis       = Some("A shy teenager becomes a virtual-world pop star."),
        cast           = Seq("Kaho Nakamura", "Ryo Narita"),
        director       = Seq("Mamoru Hosoda"),
        runtimeMinutes = Some(124),
        releaseYear    = Some(2021),
        countries      = Seq("Japonia"),
        genres         = Seq("Animacja", "Dramat"),
        posterUrl      = Some("https://img.example/belle.jpg"),
        trailerUrl     = Some("https://www.youtube.com/watch?v=abcdef12345"),
        filmUrl        = Some("https://cinema-city.pl/belle"),
        showtimes      = Seq(Showtime(
          LocalDateTime.parse("2026-06-28T18:30"),
          bookingUrl = Some("https://book.example/cc"),
          room       = Some("Sala 4"),
          format     = List("2D", "NAP")))),
      HeliosMagnolia -> SourceData(
        title     = Some("Belle"),
        filmUrl   = Some("https://helios.pl/belle"),
        showtimes = Seq(Showtime(
          LocalDateTime.parse("2026-06-28T20:00"),
          bookingUrl = Some("https://book.example/helios"),
          format     = List("ATMOS"))))))

    val ctrl = TestMovieController.build(Seq(("Belle", Some(2021), rich)), Mode.Dev)._1
    val html = contentAsString(ctrl.debugReadModel().apply(FakeRequest(GET, "/debug/readmodel")))

    // New column header + the distinct-cinema count for the row.
    html should include("<th>Cinemas</th>")
    html should include("""class="num cinemas """)   // the per-row Cinemas cell
    html should include(""">2</td>""")               // its distinct-venue count

    // Every resolved-movie field surfaced on expand.
    html should include("Ryu to Sobakasu no Hime")               // originalTitle
    html should include("124")                                   // runtimeMinutes
    html should include("Japonia")                               // countries
    html should include("Mamoru Hosoda")                         // directors
    html should include("Kaho Nakamura")                         // cast
    html should include("Animacja")                              // genres
    html should include("A shy teenager becomes a virtual-world pop star.") // synopsis
    html should include("https://img.example/belle.jpg")         // posterUrl
    html should include("youtube.com/embed/abcdef12345")         // trailerUrls (embed-mapped)

    // Every per-cinema / per-showtime field surfaced on expand.
    html should include("https://cinema-city.pl/belle")          // filmUrl
    html should include("https://book.example/cc")               // showtime bookingUrl
    html should include("Sala 4")                                // showtime room
    html should include("NAP")                                   // showtime format token
  }

  it should "404 in production like the rest of /debug" in {
    val result = buildController(Mode.Prod).debugReadModel().apply(FakeRequest(GET, "/debug/readmodel"))
    status(result) shouldBe NOT_FOUND
  }

  // Unlike the other /debug pages, rehydrate runs in every mode (it reconciles a
  // live prod instance's caches) and mutates state — so it's gated by the admin
  // allowlist instead of left open, even in prod.
  private val rehydrateRequest = FakeRequest(POST, "/wroclaw/debug/rehydrate")

  "POST /…/debug/rehydrate" should "401 an anonymous request" in {
    val ctrl = TestMovieController.build(records, Mode.Prod)._1
    status(ctrl.rehydrate("wroclaw").apply(rehydrateRequest)) shouldBe UNAUTHORIZED
  }

  it should "403 a logged-in user not on the allowlist" in {
    val ctrl = TestMovieController.build(records, Mode.Prod,
      adminAction = TestAdminAction(allow = Set("someone-else@example.com")))._1
    status(ctrl.rehydrate("wroclaw").apply(
      rehydrateRequest.withSession("userId" -> TestAdminAction.AdminUserId))) shouldBe FORBIDDEN
  }

  it should "reload the caches for an allowlisted admin" in {
    val ctrl = TestMovieController.build(records, Mode.Prod)._1
    val result = ctrl.rehydrate("wroclaw").apply(
      rehydrateRequest.withSession("userId" -> TestAdminAction.AdminUserId))
    status(result) shouldBe OK
    contentAsString(result) should include("rehydrated")
  }

  // ── Per-row re-enrich button (dev-only) ──────────────────────────────────────
  private val reenrichRequest = FakeRequest(POST, "/debug/reenrich")

  "POST /debug/reenrich" should "enqueue a ResolveTmdb task with the row's title + year in dev" in {
    val q    = new InMemoryTaskQueue
    val ctrl = TestMovieController.build(records, Mode.Dev, taskQueue = q)._1

    val result = ctrl.reenrich("Belle", Some(2021)).apply(reenrichRequest)
    status(result) shouldBe OK
    (Json.parse(contentAsString(result)) \ "enqueued").as[Boolean] shouldBe true

    val active = q.monitor().active
    active.map(_.taskType) shouldBe Seq(TaskType.ResolveTmdb.name)
    active.head.dedupKey shouldBe EnrichTaskKeys.resolveTmdbDedup("Belle", Some(2021))
  }

  it should "report duplicate on a second enqueue for the same film" in {
    val q    = new InMemoryTaskQueue
    val ctrl = TestMovieController.build(records, Mode.Dev, taskQueue = q)._1
    ctrl.reenrich("Belle", Some(2021)).apply(reenrichRequest)
    val second = ctrl.reenrich("Belle", Some(2021)).apply(reenrichRequest)
    (Json.parse(contentAsString(second)) \ "duplicate").as[Boolean] shouldBe true
    q.monitor().active.size shouldBe 1
  }

  it should "400 a request with no title (and enqueue nothing)" in {
    val q    = new InMemoryTaskQueue
    val ctrl = TestMovieController.build(records, Mode.Dev, taskQueue = q)._1
    status(ctrl.reenrich("", None).apply(reenrichRequest)) shouldBe BAD_REQUEST
    q.monitor().active shouldBe empty
  }

  it should "404 in production (no enqueue) like the rest of /debug" in {
    val q    = new InMemoryTaskQueue
    val ctrl = TestMovieController.build(records, Mode.Prod, taskQueue = q)._1
    status(ctrl.reenrich("Belle", Some(2021)).apply(reenrichRequest)) shouldBe NOT_FOUND
    q.monitor().active shouldBe empty
  }
}
