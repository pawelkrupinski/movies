package services.tasks

import settings.ScrapeInitialDelay

import tools.{ScheduledTick, Stoppable, TickRecorder}
import play.api.Logging
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}

import java.time.{Clock, Instant}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Periodically enqueues a `ScrapeCinema` task for every cinema that is due under
 * the shared [[DueWindow]] — its phase-window boundary has passed since the last
 * successful scrape, or it has never been scraped. The window's period is the
 * freshness setting (`KINOWO_SCRAPE_FRESHNESS_MINUTES`, default 60min) — set per
 * country in that worker's k3s overlay, so the rate is NOT uniform across the
 * fleet: each country's value is sized so its PACED sweep fits inside it, which
 * on a chunked country with a big roster is many times PL's hour. The overlays
 * hold the live values and `WorkerScrapeCadenceConfigSpec` asserts the sweep
 * still fits each one — read them there, not from a number in this comment, which
 * is how the figures quoted here went stale before. Each cinema's boundary sits at its
 * own phase offset, spaced by what the cinema costs the queue ([[CostSpacedPhaseOffset]]),
 * so a country's scrape load spreads evenly across the period instead of all falling
 * due together and scraping in a lockstep wave. Enqueue is deduped by the queue, so a
 * cinema with a task already waiting/working isn't queued twice; the handler
 * re-checks the SAME `DueWindow` and skips only if a concurrent run already
 * refreshed it this window (never a still-due task — that churn is what [[DueWindow]] fixes).
 *
 * Instead of re-scraping every cinema back-to-back in a continuous loop, the
 * worker scrapes a cinema at most once per window, and a failed scrape
 * (which doesn't mark freshness) is naturally retried on the next reaper tick.
 *
 * When a backlog makes more cinemas due than `maxEnqueuePerTick`, the tick
 * enqueues the MOST-OVERDUE first (oldest last-scrape, never-scraped first), so a
 * credit-throttled worker draining slowly doesn't keep re-serving the head of the
 * fixed city list and starving the tail — see [[DueScrapes]]. What a tick may admit is
 * [[ScrapeAdmission]]'s, the tick's work [[ScrapeTick]]'s; this class is the schedule.
 *
 * On a multi-machine worker each tick is gated by a cluster-wide occurrence
 * claim ([[ScheduledRunStore]]) keyed by the tick's minute, so a given minute's
 * stale-cinema enqueue runs on one machine, rotating — not on every machine.
 */
class ScrapeReaper(
  due:       DueScrapes,
  scrapeTick: ScrapeTick,
  interval:  ScrapeReaper.TickInterval = ScrapeReaper.TickInterval(1.minute),
  // A small extra spacing before the (now post-hydrate) first tick, so it doesn't
  // land on the same instant as the cache hydrate finishing. Defaults to 0 so the
  // tests that drive `tick()` directly are unaffected.
  initialDelay: ScrapeInitialDelay = ScrapeInitialDelay(0.seconds),
  // How long each readiness wait blocks before logging and waiting again. We never
  // tick against a not-ready mirror (that was the boot storm) — readiness itself
  // now completes only once a hydrate SUCCEEDS or its bounded retry budget is spent
  // (see MongoFreshnessStore.hydrateInPhases), so this just paces the holding log.
  readyTimeout: ScrapeReaper.ReadyTimeout = ScrapeReaper.ReadyTimeout(30.seconds),
  runStore: ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:    Clock,
  // Runs the readiness wait and the ticks; a spec hands in a stepped one to cross the delays by hand.
  scheduler: ScheduledExecutorService,
  // Where the schedule is reported started; the tick's work reports each run completed ([[ScrapeTick]]).
  ticks: TickRecorder = TickRecorder.Unrecorded
) extends Stoppable with Logging {

  def start(): Unit = {
    if (due.rosterSize == 0) { logger.info("ScrapeReaper: no cinemas; not starting."); return }
    // Defer onto the scheduler thread so we can block it on the freshness hydrate
    // without holding up boot wiring; it then schedules the periodic ticks.
    scheduler.execute(() => ScheduledTick.logged("ScrapeReaper start", logger)(awaitReadyThenStart()))
    // Armed now, not once the freshness mirror is ready: a hydrate that never finishes is no scraping too.
    ticks.started(ScrapeTick.Name, interval.value, initialDelay.value)
    logger.info(s"ScrapeReaper started over ${due.rosterSize} cinemas, first tick after freshness hydrate then ${initialDelay.value.toSeconds}s, every ${interval.value.toSeconds}s.")
  }

  // Wait until the scrape freshness stamps are actually loaded, THEN begin the
  // periodic ticks. Readiness completes only once a hydrate SUCCEEDS (or its
  // bounded retry budget is spent — see MongoFreshnessStore.hydrateInPhases), so a
  // transient Mongo timeout no longer green-lights an empty mirror. We keep waiting
  // rather than ticking against a not-yet-hydrated mirror: ticking then would read
  // every cinema as stale and enqueue all ~300 at once — the boot storm that
  // drained the shared-CPU credit balance and slowed the next restart's hydrate,
  // storming again.
  private def awaitReadyThenStart(): Unit = {
    while (!Try(Await.ready(due.whenReady, readyTimeout.value)).isSuccess)
      logger.info("ScrapeReaper: freshness mirror still hydrating; holding scrape ticks (no cold re-scrape).")
    // The schedule records only that it started: the tick's work completes it ([[ScrapeTick]]).
    scheduler.scheduleWithFixedDelay(() => ScheduledTick.recorded(ScrapeTick.Name, interval.value, logger,
      TickRecorder.completedByTheWork(ticks))(tickIfClaimed()), initialDelay.value.toMillis, interval.value.toMillis, TimeUnit.MILLISECONDS)
  }

  /** Tick only if this machine wins the current minute's occurrence claim —
   *  otherwise another machine is enqueuing this window's stale cinemas, so
   *  skip (a no-op, which completes). Returns the number enqueued (0 when the claim was lost). */
  private[tasks] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("scrape", clock.millis(), interval.value, 0.seconds)
    if (runStore.claim(key)) tick() else { scrapeTick.skipped(); 0 }
  }

  /** One tick's work ([[ScrapeTick]]). Package-private so tests can drive it directly — bypasses the occurrence claim. */
  private[tasks] def tick(now: Instant = clock.instant()): Int = scrapeTick.run(now)

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object ScrapeReaper {

  /** How often the reaper wakes to enqueue what has come due. */
  final case class TickInterval(value: FiniteDuration) extends AnyVal
  /** How long the first tick waits for the freshness store to hydrate. */
  final case class ReadyTimeout(value: FiniteDuration) extends AnyVal
}
