package services

import play.api.Logging
import services.cinemas.CinemaScraper
import services.movies.MovieCache
import services.events.{EventBus, MovieRecordCreated}
import tools.DaemonExecutors

import java.io.IOException
import java.util.concurrent.{TimeUnit, TimeoutException}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutorService, Future}

/** Scrape scheduler. Hits every cinema every 5 minutes, hands the result
 *  to `MovieCache.recordCinemaScrape`, and publishes a `MovieRecordCreated` event
 *  per movie so the enrichment pipeline can pick them up. Holds no
 *  in-memory state of its own — `MovieCache` is the read path.
 *
 *  Per CLAUDE.md OCP guidance: adding a new cinema is a new `CinemaScraper`
 *  wired in `AppLoader`; this class never names a specific cinema. */
class ShowtimeCache(
  scrapers:   Seq[CinemaScraper],
  bus:        EventBus,
  movieCache: MovieCache,
  // Defaults to a dedicated unbounded pool so tests/scripts construct it as
  // before; `Wiring` injects a shared-budget EC (sub-capped at 2) so a burst of
  // slow scrapes can't peg the box. See `SharedExecutionBudget`.
  ec:          ExecutionContextExecutorService = DaemonExecutors.virtualThreadEC("showtime-fetch"),
  // Safety net so one pathologically-stuck pass can't stall the continuous loop
  // forever. Generous — a normal pass (even ~48 cinemas at 2-at-a-time) finishes
  // well within this. Injectable so tests can exercise the timeout path.
  passTimeout: FiniteDuration = 30.minutes
) extends Stoppable with Logging {

  private val scheduler = DaemonExecutors.scheduler("showtime-cache-refresh")
  // Brief breather between back-to-back passes — essentially "start the next one
  // as soon as this finishes" without a hot loop.
  private val InterPassDelay: FiniteDuration = 5.seconds

  /** Run scrape passes back-to-back: one full pass over every cinema, and the
   *  next pass starts only once the previous one finishes — there is no fixed
   *  clock. `scheduleWithFixedDelay` measures its gap from completion (not from
   *  start), and `runPass` BLOCKS until the whole pass settles, so passes can
   *  never overlap and a slow pass can't be lapped by the next tick (the old
   *  every-5-min `scheduleAtFixedRate` re-submitted all N on top of an unfinished
   *  pass, which on a busy box pegged the vCPU indefinitely). The effective
   *  refresh interval self-adjusts to how long a pass actually takes.
   *
   *  Each cinema fetch publishes its own `MovieRecordCreated` events as it
   *  completes, so enrichment starts without waiting for the N-cinema barrier. */
  def start(): Unit = {
    logger.info(s"Starting — commit ${Option(System.getenv("COMMIT_SHA")).getOrElse("unknown")}")
    if (scrapers.isEmpty) return
    logger.info(s"Scrape: continuous passes over ${scrapers.size} cinemas, " +
                s"${InterPassDelay.toSeconds}s between passes (next starts when the last finishes).")
    scheduler.scheduleWithFixedDelay(
      () => runPass(),
      0L, InterPassDelay.toMillis, TimeUnit.MILLISECONDS
    )
  }

  /** One full pass: submit every scraper onto the fetch EC and block until they
   *  have all settled. Never throws — a per-scraper failure is already caught in
   *  `refreshOne`, and a whole-pass timeout is caught here — so the
   *  `scheduleWithFixedDelay` loop keeps running. */
  private[services] def runPass(): Unit =
    try {
      Await.ready(Future.sequence(submitAllScrapers())(using implicitly, ec), passTimeout)
      ()
    } catch {
      case _: TimeoutException =>
        logger.warn(s"Scrape pass exceeded ${passTimeout.toMinutes}min — starting a fresh pass.")
      case e: Throwable =>
        logger.warn(s"Scrape pass aborted: ${e.getMessage}")
    }

  /** Run every scraper once on the fetch EC and block until they've all
   *  completed. After this returns, every cinema's `MovieRecordCreated`
   *  events have been published synchronously and the caller can drain
   *  the downstream worker pools (`MovieService`, `*Ratings`, …) without
   *  racing the scrape. */
  def runOnce(): Unit = {
    Await.ready(Future.sequence(submitAllScrapers())(using implicitly, ec), Duration.Inf)
    ()
  }

  /** Submit every scraper on the fetch EC. Returns the futures so callers
   *  that want to block (`runOnce`) can; the periodic path discards them.
   *  Per-scraper failures are caught inside `refreshOne` and never reach
   *  the future. */
  private def submitAllScrapers(): Seq[Future[Unit]] =
    scrapers.map(s => Future(refreshOne(s))(using ec))

  def stop(): Unit = {
    scheduler.shutdown()
    ec.shutdown()
  }

  // Network/HTTP errors from external cinema sites — expected and not actionable.
  // Log at WARN (invisible to Sentry) instead of ERROR.
  private[services] def isTransientHttpError(e: Throwable): Boolean = e match {
    case _: IOException       => true  // timeouts, SSL, connection errors
    case _: TimeoutException  => true  // Future timeouts
    case e: RuntimeException  =>
      val msg = Option(e.getMessage).getOrElse("")
      // "HTTP <code> for <method> <url>"          — RealHttpFetch.checkStatus
      // "All N backends failed for <verb> <url>"  — FallbackHttpFetch.tryEach
      msg.startsWith("HTTP ") ||
        (msg.startsWith("All ") && msg.contains(" backends failed for "))
    case _                    => false
  }

  private def refreshOne(scraper: CinemaScraper): Unit = {
    val cinema = scraper.cinema
    logger.debug(s"Refreshing ${cinema.displayName}")
    val t0     = System.currentTimeMillis()
    try {
      val movies  = scraper.fetch()
      val elapsed = System.currentTimeMillis() - t0
      // Publish against the *canonical* CacheKey that recordCinemaScrape
      // actually wrote each slot to — when the redirect absorbed this
      // scrape's (title, year) into an existing sibling row, the bus event
      // names the sibling's key so the TMDB stage doesn't run a second
      // time for a phantom row.
      //
      // Suppress the event when this (cinema, raw title, raw year) tuple
      // has already been scraped onto this row: every downstream listener
      // is idempotent (re-checking state before doing work), so a steady-
      // state tick where nothing changed would dispatch hundreds of no-op
      // events per 5-minute interval. Listeners still get fresh events
      // when a cinema reports a new title spelling, a year correction, or
      // when a brand new film shows up.
      val touched     = movieCache.recordCinemaScrape(cinema, movies)
      val publishable = touched.count(_._3)
      logger.info(s"Refreshed ${cinema.displayName}: ${movies.size} entries in ${elapsed}ms ($publishable new)")
      touched.foreach { case (cm, key, isNew) =>
        if (isNew)
          bus.publish(MovieRecordCreated(key.cleanTitle, key.year, cm.movie.originalTitle, if (cm.director.nonEmpty) Some(cm.director.mkString(", ")) else None))
      }
    } catch {
      case e: Exception =>
        val elapsed = System.currentTimeMillis() - t0
        if (isTransientHttpError(e))
          logger.warn(s"Failed to refresh ${cinema.displayName} after ${elapsed}ms: ${e.getMessage}")
        else
          logger.error(s"Failed to refresh ${cinema.displayName} after ${elapsed}ms", e)
    }
  }
}
