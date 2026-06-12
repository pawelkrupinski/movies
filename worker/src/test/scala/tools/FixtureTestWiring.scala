package tools

import clients.tools.FakeHttpFetch
import services.cinemas.CinemaScrapeRunner
import services.events.MovieRecordCreated
import services.movies.InMemoryMovieRepo
import services.readmodel.{InMemoryReadModelRepo, ReadModelReader, ReadModelWriter, WebReadModel}

class FixtureTestWiring(val fixture: String) extends TestWiring {
  override lazy val httoFetch: HttpFetch = new FakeHttpFetch(fixture)
  override lazy val movieRepo = new InMemoryMovieRepo()

  // Mongo-free read model: the worker projects the scraped corpus into this
  // in-memory store, and the web's `WebReadModel` serves from it — the same
  // worker→read-model→web seam as production, minus Mongo. Specs build their
  // `MovieControllerService` from `webReadModel` (not the raw cache).
  override lazy val readModelRepo: ReadModelReader & ReadModelWriter = new InMemoryReadModelRepo()
  lazy val webReadModel = new WebReadModel(readModelRepo)

  // Pin Helios's REST date to the fixture's capture day when the `fixture` dir
  // is named `dd-MM-yyyy` (e.g. "08-06-2026"). Helios bakes the date window into
  // its `/screening` + `/event` URLs; without this the live `LocalDate.now`
  // makes those URLs miss the recorded fixtures, dropping Helios room/format
  // enrichment and breaking the whole-corpus snapshot on every day after
  // capture. Fixtures named for something else ("multikino") aren't date-keyed,
  // so fall back to the real date for them.
  override protected def heliosToday: java.time.LocalDate =
    scala.util.Try(
      java.time.LocalDate.parse(fixture, java.time.format.DateTimeFormatter.ofPattern("dd-MM-yyyy"))
    ).getOrElse(super.heliosToday)

  // Route Multikino through the same `FakeHttpFetch` as every other cinema —
  // single override point. The base `TestWiring` inherits production's
  // `MultikinoClient.fetchFor(httoFetch)` so `ClientIntegrationSpec`'s
  // live-network smoke still goes through Zyte.
  override lazy val multikinoFetch: HttpFetch = httoFetch

  // Same single override point for biletyna (Kino Kameralne): replay the
  // fixture rather than hitting Zyte. Without this, CI — where ZYTE_API_KEY is
  // set — routes the fixture-replay scrape through real Zyte → biletyna,
  // breaking hermetic end-to-end specs (FilmScheduleEndToEndSpec).
  override lazy val biletynaFetch: HttpFetch = httoFetch

  /** Record EVERY cinema first, THEN publish all the create events — load-bearing
   *  for a deterministic single-pass snapshot, not arbitrary scaffolding.
   *  Production publishes `MovieRecordCreated` INLINE as each cinema lands and
   *  relies on MANY 5-min passes (+ the daily retry) to converge; in ONE pass,
   *  inline publish lets the async enrichment pool mutate (rekey/resolve) a row
   *  WHILE a later cinema is still merging into it, so even a single-threaded
   *  scrape comes out order-dependent (the events-seen count wobbles run to run).
   *  Recording all cinemas before publishing settles every row to its final
   *  scraped shape first. The order-INDEPENDENCE this side-steps is proved
   *  directly and exhaustively by `ScrapeOrderDeterminismSpec`, which shuffles
   *  cinema + event order under a jittered fetch clock and asserts byte-identical
   *  records and rows across the whole corpus. */
  def runOneScrapeTick(): Unit = {
    val created = collection.mutable.ListBuffer.empty[MovieRecordCreated]
    cinemaScrapers.foreach { scraper =>
      try {
        val touched = movieCache.recordCinemaScrape(scraper.cinema, scraper.fetch())
        created ++= CinemaScrapeRunner.eventsFor(touched)
      } catch { case _: Exception => () }
    }
    created.foreach(eventBus.publish)
  }

  /** Convenience: scrape every cinema once, drain the cascade, then run
   *  the daily `UnscreenedCleanup` pass. After this returns the cache is in
   *  the same shape it would be ~20s into production boot, so the rest of
   *  the test can assert against `movieCache.snapshot()` directly (the serving
   *  transform — `MovieControllerService.toSchedules` — is the web app's job
   *  now and is tested there). */
  def bootStartup(): Unit = {
    runOneScrapeTick()
    drainServices()
    unscreenedCleanup.removeUnscreened()
    // Project the settled corpus into the read model and warm the web view, so
    // a spec can serve via `webReadModel` exactly as the web app does. Done as a
    // one-shot reconcile + reload (no change-stream/scheduler) to keep the test
    // deterministic and thread-free.
    readModelProjector.reconcile()
    webReadModel.reload()
  }

  /** Settle the cache to its deterministic steady state. The parallel scrape
   *  (`showtimeCache.runOnce`) publishes enrichment INLINE as each cinema lands,
   *  so a film's TMDB/ratings can resolve against a partially-merged row; in
   *  production those rows settle over successive 5-min passes (+ the daily TMDB
   *  retry). A single test pass can't wait for that, so re-run enrichment
   *  synchronously against the now-settled rows — the same belt-and-braces sweep
   *  the fixture recorder uses — making the snapshot a pure function of the
   *  fixtures rather than of thread scheduling. Deterministic title order so the
   *  sweep itself introduces no ordering of its own. */
  def converge(): Unit = {
    // 1. Collapse spelling/year variants a concurrent scrape/enrichment left
    //    behind FIRST, so every row is under its canonical key BEFORE we
    //    re-enrich. Otherwise the title-keyed enrichment (esp. Filmweb's fuzzy
    //    title/director SEARCH) would run against an order-dependent spelling
    //    and land an order-dependent result. `movieService.settle()` is the SAME
    //    canonicalisation the worker now runs on a timer — so this harness
    //    exercises the production settle path, not a test-only copy of it.
    movieService.settle()
    // 2. Re-run enrichment synchronously against the now-canonical, settled rows
    //    — the same belt-and-braces sweep the fixture recorder uses — so the
    //    snapshot is a pure function of the fixtures, not of thread scheduling.
    //    Deterministic title order so the sweep itself adds no ordering.
    movieCache.snapshot()
      .map(r => (r.title, r.year))
      .sortBy { case (t, y) => (t, y.getOrElse(Int.MinValue)) }
      .foreach { case (t, y) =>
        // Swallow a missing-fixture throw the SAME way the async enrichment
        // path does (MovieService logs "Giving up on TMDB …" and moves on):
        // a row whose TMDB id has no recorded `external_ids` (NTLive theatre
        // captures share id 203912, which has no IMDb cross-reference) would
        // otherwise abort the whole sweep.
        try fullySyncOne(t, y) catch { case _: Exception => () }
      }
    // 3. Re-collapse: the TMDB stage can rekey a no-year row onto a resolved
    //    year, briefly re-introducing a spelling/year variant — exactly the
    //    "Dzień objawienia" shape the worker's periodic settle exists to fix.
    movieService.settle()
  }
}
