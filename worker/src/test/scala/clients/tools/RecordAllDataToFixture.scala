package clients.tools

import services.cinemas.{MultikinoClient, ZyteFallback}
import services.movies.InMemoryMovieRepo
import tools.{DaemonExecutors, HttpFetch, RealHttpFetch, TestWiring}

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutorService, Future}

/**
 * One-shot fixture recorder. Runs the full production pipeline once
 * against real upstream APIs; every HTTP response flows through
 * `RecordingHttpFetch`, which writes a fixture file under
 * `test/resources/fixtures/$captureDate/` per unique URL. After this
 * exits, the fixture tree contains every byte every client would fetch
 * in normal operation, captured deterministically.
 *
 * Wait semantics — each step blocks until its outputs are fully on disk:
 *
 *   1. `scrapeAllOnce()` — submits every scraper to a bounded executor in
 *      parallel and joins on the resulting `Future`s. Each runs the shared
 *      `cinemaScrapeRunner` (fetch → `recordCinemaScrape` → publish), so it
 *      returns only after every `scraper.fetch()` HTTP call has come back,
 *      the result has been folded into the cache, and
 *      `bus.publish(MovieDetailsComplete(...))` has fired for each `isNew`
 *      slot. The bus listeners run synchronously and dispatch to the
 *      downstream worker pools before publish returns.
 *
 *   2. `drainServices()` — stops the cascade in producer→consumer
 *      order (`cascadeDrainOrder` on `Wiring`: `movieService` →
 *      `imdbIdResolver` → the four `*Ratings`). Each `stop()` shuts the
 *      worker pool down and `awaitTermination(15s)`s so all queued
 *      tasks complete. During each drain, the queued tasks publish
 *      their own bus events (`TmdbResolved`, `ImdbIdMissing`,
 *      `ImdbIdResolved`), which synchronously dispatch to the next
 *      pool; that pool drains in the next step. After `drainServices()`
 *      returns every cache row has:
 *        - `tmdbId`/`imdbId`/`originalTitle` from TMDB
 *        - IMDb rating (GraphQL CDN) or suggestion-recovered `imdbId`
 *        - MC URL + metascore (slug probe + canonical-page scrape)
 *        - RT URL + Tomatometer (slug probe + canonical-page scrape)
 *        - Filmweb URL + rating (search + info + preview + rating)
 *      …each HTTP call recorded into the fixture tree.
 *
 *   3. `unscreenedCleanup.removeUnscreened()` — exercise the daily
 *      cleanup pass too; matches what the live deploy does ~20 s after
 *      boot, so fixtures for any "row with no current screenings" code
 *      path are captured.
 *
 *   4. `unscreenedCleanup.stop()` / `movieRepo.close()` — close the
 *      remaining schedulers + Mongo client. The pool threads are
 *      daemon-flagged, so the JVM would exit cleanly without this, but if
 *      anyone ever flips a thread to non-daemon, the explicit shutdown
 *      keeps the recording correct.
 */
object RecordAllDataToFixture extends TestWiring {
  // The fixture dir name = the capture date. The CI refresh job (and the daily
  // country-fixture artifact job) `sed`-rewrite this one literal to the run
  // date; every RecordingHttpFetch below keys its tree off it.
  val captureDate = "08-06-2026"

  override lazy val movieRepo = new InMemoryMovieRepo()
  override lazy val httoFetch = new RecordingHttpFetch(captureDate, new RealHttpFetch())

  // Multikino and Kino Kameralne (biletyna) sit behind a WAF that blocks our
  // datacenter IP, so production routes them through a Zyte-primary → direct
  // chain (`MultikinoClient.fetchFor` / `ZyteFallback.fetchFor`). The Zyte leg
  // fetches through its OWN HttpClient, so when recording is the chain's inner
  // `direct` fallback (the production wiring), a Zyte-served response bypasses
  // the recorder entirely — the scrape succeeds but nothing lands on disk, and
  // the corpus silently lacks every `www.multikino.pl` / biletyna fixture.
  //
  // Fix: build the production chain over a plain RealHttpFetch and wrap the
  // WHOLE chain in recording, so the response is captured keyed by the target
  // URL no matter which leg (Zyte or direct) served it. Guarded by
  // `RecorderZyteCaptureSpec`.
  override lazy val multikinoFetch: HttpFetch =
    new RecordingHttpFetch(captureDate, MultikinoClient.fetchFor(new RealHttpFetch()))
  override lazy val biletynaFetch: HttpFetch =
    new RecordingHttpFetch(captureDate, ZyteFallback.fetchFor(new RealHttpFetch()))

  // TestWiring stubs the TMDB key to "test-api-key" (fine for replay, where the
  // fixture filename strips api_key). But RECORDING fires the real request, so
  // it needs the real key from the environment — otherwise every TMDB search
  // 401s and no enrichment is captured. The recorded filename is still
  // key-agnostic (RecordingHttpFetch strips api_key), so replay is unaffected.
  override lazy val tmdbClient: clients.TmdbClient =
    new clients.TmdbClient(httoFetch, apiKey = sys.env.get("TMDB_API_KEY"))

  /** Scrape every cinema once in parallel, blocking until all have settled.
   *  Each scraper runs the shared `cinemaScrapeRunner` (record + publish), so
   *  every `MovieDetailsComplete` is published synchronously before this returns
   *  and the caller can drain the downstream pools without racing the scrape. */
  private def scrapeAllOnce(): Unit = {
    val ec: ExecutionContextExecutorService = DaemonExecutors.boundedEC("record-scrape", 8)
    try Await.ready(
      Future.sequence(cinemaScrapers.map(s =>
        Future(scala.util.Try(cinemaScrapeRunner.run(s)))(using ec)))(using implicitly, ec),
      Duration.Inf)
    finally ec.shutdown()
    ()
  }

  def main(args: Array[String]): Unit = {
  // 1. Production-shape pass: every cinema scrape fires (bare), the enqueued
  //    EnrichDetails tasks are drained so each film's detail page is fetched +
  //    recorded, then every bus event cascades through the worker pools and the
  //    cascade drains in producer→consumer order.
  scrapeAllOnce()
  enrichDetailsSync()
  drainServices()

  // 2. Safety-net pass: synchronously force every cache row through the
  //    full pipeline. Belt-and-braces against the async path dropping
  //    work via the retry-scheduler — when a transient TMDB rate-limit
  //    blip during the first scrape burst schedules a retry with a
  //    30-s+ backoff, the `movieService.worker` has already been shut
  //    down by the time the retry fires, the `worker.execute` throws
  //    `RejectedExecutionException`, and the row's enrichment is lost
  //    silently. The sync pass re-runs TMDB / IMDb / MC / RT / FW on
  //    the calling thread (no worker pools, no scheduled retries), so
  //    every fixture file every client would ever touch ends up on disk
  //    exactly once.
  //
  //    Wrapped in `Try` per row so a single bad upstream (HTTP timeout,
  //    parse error, 5xx) skips that row instead of killing the whole
  //    loop. `RealHttpFetch` has a 30-s per-request timeout so a hung
  //    server can't pin the loop forever.
  //    Parallelised across `SyncPassWorkers` threads. CLAUDE.md's
  //    "5–10 workers for a single API" cap is the right rule of thumb,
  //    but the per-row work fans out across multiple APIs (TMDB / IMDb /
  //    MC / RT / FW). Five concurrent rows × ~10 HTTP calls each peaks
  //    at ~50 simultaneous in-flight requests, well under TMDB's
  //    documented 50 req/s and inside the empirical envelope for the
  //    scrape-target sites. Bump or trim if a specific upstream starts
  //    rate-limiting.
  //
  //    Cache writes (`cache.put`, `cache.putIfPresent`) are already
  //    thread-safe via Caffeine + per-tmdbId locks; `RecordingHttpFetch`
  //    serialises file writes per path through the filesystem. Output
  //    interleaves at line granularity because `println` is synchronised
  //    on `System.out` — each row prints one complete line on
  //    completion, which keeps the log readable even at 5× concurrency.
  val SyncPassWorkers = 5
  val rows  = movieCache.snapshot()
  val total = rows.size
  println(s"Sync-pass: re-running enrichment for $total cache row(s) across $SyncPassWorkers worker(s)…")
  val startedAt = System.currentTimeMillis()
  implicit val syncPool: ExecutionContextExecutorService = DaemonExecutors.boundedEC("sync-pass", SyncPassWorkers)
  val done = new AtomicInteger(0)

  val futures = rows.map { row =>
    Future {
      val t0       = System.currentTimeMillis()
      val outcome  = scala.util.Try(fullySyncOne(row.title, row.year))
      val elapsed  = System.currentTimeMillis() - t0
      val n        = done.incrementAndGet()
      val label    = f"  [$n%3d/$total%3d]"
      val title    = s"'${row.title}' (${row.year.getOrElse("?")})"
      outcome match {
        case scala.util.Success(_)  => println(f"$label done in $elapsed%5d ms · $title")
        case scala.util.Failure(ex) => println(f"$label FAIL  in $elapsed%5d ms · $title — ${ex.getMessage}")
      }
    }
  }

  // Block until every future has completed. `Duration.Inf` rather than a
  // fixed cap so a genuinely slow upstream doesn't truncate the recording
  // mid-run; per-request HTTP timeouts (30 s in `RealHttpFetch`) keep any
  // single call bounded.
  Await.result(Future.sequence(futures), Duration.Inf)
  syncPool.shutdown()
  println(f"Sync-pass: done in ${(System.currentTimeMillis() - startedAt) / 1000.0}%.1f s.")

  // 3. Daily cleanup pass — exercises the "rows with no current
  //    screenings" code path so its (lack of) HTTP is captured too.
  unscreenedCleanup.removeUnscreened()

  // 4. Close remaining schedulers + Mongo.
  unscreenedCleanup.stop()
  movieRepo.close()
  }
}
