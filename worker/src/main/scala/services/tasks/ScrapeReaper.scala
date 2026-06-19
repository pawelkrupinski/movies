package services.tasks

import play.api.Logging
import services.Stoppable
import services.cinemas.CinemaScraper
import services.freshness.{Freshness, FreshnessKind, FreshnessStore}
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.DaemonExecutors

import java.time.{Clock, Instant}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Try}

/**
 * Periodically enqueues a `ScrapeCinema` task for every cinema that is due under
 * the shared [[DueWindow]] — its phase-window boundary has passed since the last
 * successful scrape, or it has never been scraped. The window's period is the
 * freshness setting (default 30min, `KINOWO_SCRAPE_FRESHNESS_MINUTES`); each
 * cinema's boundary sits at a deterministic phase offset hashed from its key, so
 * the ~300 cinemas spread evenly across the period instead of all falling due
 * together and scraping in a lockstep wave. Enqueue is deduped by the queue, so a
 * cinema with a task already waiting/working isn't queued twice; the handler
 * re-checks the SAME `DueWindow` and skips only if a concurrent run already
 * refreshed it this window (never a still-due task — that churn is what [[DueWindow]] fixes).
 *
 * Instead of re-scraping every cinema back-to-back in a continuous loop, the
 * worker scrapes a cinema at most once per window, and a failed scrape
 * (which doesn't mark freshness) is naturally retried on the next reaper tick.
 *
 * On a multi-machine worker each tick is gated by a cluster-wide occurrence
 * claim ([[ScheduledRunStore]]) keyed by the tick's minute, so a given minute's
 * stale-cinema enqueue runs on one machine, rotating — not on every machine.
 */
class ScrapeReaper(
  scrapers:  Seq[CinemaScraper],
  queue:     TaskQueue,
  freshness: FreshnessStore,
  // The shared due schedule (each cinema scraped once per its freshness window,
  // phase-spread across it so all ~300 don't fall due together). The SAME instance
  // must back `ScrapeCinemaHandler` so this enqueue gate and that pickup re-gate
  // agree on what's due — see [[DueWindow]].
  dueWindow: DueWindow = new DueWindow(Freshness.defaultScrapeTtl),
  interval:  FiniteDuration = 1.minute,
  // A small extra spacing before the (now post-hydrate) first tick, so it doesn't
  // land on the same instant as the cache hydrate finishing. Defaults to 0 so the
  // tests that drive `tick()` directly are unaffected.
  initialDelay: FiniteDuration = 0.seconds,
  // Cap on how long the first tick waits for the freshness mirror to load its
  // scrape stamps. Past it we tick anyway — degrading to the old re-scrape-all
  // behaviour — rather than never scraping if a hydrate wedges.
  readyTimeout: FiniteDuration = 30.seconds,
  // Cap on how many stale cinemas a single tick enqueues. After a restart every
  // cinema can be stale at once; enqueuing all ~300 lets the TaskWorker pool
  // drain flat-out for minutes with no idle gap, exhausting the shared-CPU
  // credit balance (the boot-storm throttle spike). Capping the per-tick batch
  // so it drains inside the tick interval leaves the pool idle between minutes,
  // letting credit recover — the backlog clears over a few ticks instead. The
  // queue dedups, so already-in-flight cinemas don't re-count against the cap.
  // Default unbounded so the tests that drive `tick()` directly are unaffected.
  maxEnqueuePerTick: Int = Int.MaxValue,
  runStore: ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:    Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("scrape-reaper")

  def start(): Unit = {
    if (scrapers.isEmpty) { logger.info("ScrapeReaper: no cinemas; not starting."); return }
    // Defer onto the scheduler thread so we can block it on the freshness hydrate
    // without holding up boot wiring; it then schedules the periodic ticks.
    scheduler.execute(() => Try(awaitReadyThenStart()))
    logger.info(s"ScrapeReaper started over ${scrapers.size} cinemas, first tick after freshness hydrate then ${initialDelay.toSeconds}s, every ${interval.toSeconds}s.")
  }

  // Block until the scrape freshness stamps are loaded (capped at `readyTimeout`),
  // THEN begin the periodic ticks. Without this the first tick can read a
  // not-yet-hydrated mirror, see every cinema as stale, and enqueue all ~300 at
  // once — the boot storm that drained the shared-CPU credit balance.
  private def awaitReadyThenStart(): Unit = {
    Try(Await.ready(freshness.whenReady(FreshnessKind.CinemaScrape), readyTimeout)) match {
      case Failure(_) =>
        logger.warn(s"ScrapeReaper: freshness stamps not ready after ${readyTimeout.toSeconds}s; first tick may re-scrape every cinema.")
      case _ => ()
    }
    scheduler.scheduleWithFixedDelay(() => Try(tickIfClaimed()), initialDelay.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
  }

  /** Tick only if this machine wins the current minute's occurrence claim —
   *  otherwise another machine is enqueuing this window's stale cinemas, so
   *  skip. Returns the number enqueued (0 when the claim was lost). */
  private[tasks] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("scrape", clock.millis(), interval, 0.seconds)
    if (runStore.claim(key)) tick() else 0
  }

  /** Enqueue every stale cinema. Package-private so tests can drive it directly —
   *  bypasses the occurrence claim. */
  private[tasks] def tick(now: Instant = clock.instant()): Int = {
    var enqueued = 0
    scrapers.iterator.takeWhile(_ => enqueued < maxEnqueuePerTick).foreach { s =>
      val key = ScrapeCinemaHandler.dedupKey(s.cinema)
      if (dueWindow.isDue(key, freshness.lastFetchedAt(key), now)) {
        if (queue.enqueue(TaskType.ScrapeCinema, key,
              Map(ScrapeCinemaHandler.CinemaKey -> s.cinema.displayName)) == EnqueueResult.Added)
          enqueued += 1
      }
    }
    if (enqueued > 0) logger.info(s"ScrapeReaper enqueued $enqueued stale cinema(s).")
    enqueued
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
