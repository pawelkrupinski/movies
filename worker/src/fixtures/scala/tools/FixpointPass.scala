package tools

import io.prometheus.metrics.model.snapshots.Labels
import services.movies.{CountingScreeningsRepository, CountingSlotsRepository}

import scala.concurrent.duration._

/**
 * One whole production tick, and the ledger that says whether it did any work.
 *
 * The fixpoint guard every pipeline spec can apply: boot a scenario, run [[run]] once
 * (the first pass after a boot may legitimately look — a heal's first sweep, a rating
 * nobody had asked for yet), then `FixpointPass.ledger(w).assertNoChurn(...)(run(w))`.
 * Over IDENTICAL input the second pass must write nothing, ask nothing, re-key nothing and
 * re-project nothing; see [[ChurnLedger]] for the history that made each axis necessary.
 *
 * "One tick" is every stage production runs on a cadence, in its order:
 *   1. scrape landing — every venue re-reports the listing it reported last time;
 *   2. the staging fold — whatever the landing diverted incubates and graduates;
 *   3. the settle — both halves, as the periodic SettleReaper runs them;
 *   4. projection — the change stream re-projects what was written (see
 *      [[attachProjector]]), then the 30-minute prune/heal/drift sweep runs;
 *   5. enrichment re-dispatch — one whole period of the TMDB re-try sweep, then the
 *      rating reaper's walk drained to quiescence.
 * Operator-triggered bulk walks are not part of a tick and are left to their own specs.
 */
object FixpointPass {

  /** The counter families that are work, not state. Gauges are excluded by construction
   *  (only counters are read); durations, latencies and census counts are excluded here
   *  because a no-op pass legitimately moves them — and so is `readmodel_project_calls`,
   *  because the prune sweep re-projects one rolling slice of the corpus every run BY
   *  DESIGN (the content-drift check). What a projection wrote is `readmodel_writes`;
   *  a heal that looks without writing (the `dfe62a96c` loop) is `readmodel_heal_checks`. */
  val WorkFamilies: Set[String] = Set(
    "kinowo_worker_merges",
    "kinowo_worker_rekeys",
    "kinowo_worker_splits",
    "kinowo_worker_readmodel_writes",
    "kinowo_worker_readmodel_films_pruned",
    "kinowo_worker_readmodel_cards_retired",
    "kinowo_worker_readmodel_catchup_rows",
    "kinowo_worker_readmodel_heals",
    "kinowo_worker_readmodel_heal_checks",
    "kinowo_worker_readmodel_card_writes",
    "kinowo_worker_readmodel_drift_writes",
    "kinowo_worker_staging_newcomer_kicks",
    "kinowo_worker_cache_rehydrate_changes",
    "kinowo_worker_tasks_enqueued",
    "kinowo_worker_movie_change_events",
    "kinowo_worker_screenings_change_events",
    "kinowo_worker_screenings_writes",
    "kinowo_worker_movie_slots_change_events",
    "kinowo_worker_resolve_retry_duplicates",
    "kinowo_worker_repository_write_failed")

  /** An enqueue of one of these is a RETRY, a REFRESH or a RE-ASK, and only the last is
   *  churn — so they are counted by [[ReaskCountingTaskQueue]], not the plain `added` series. */
  private val StampedTaskTypes: Set[String] = ReaskCountingTaskQueue.StampedTypes.map(_.name)

  /** A queue that refused a duplicate did no dispatch, a stamped task type is counted by
   *  its re-asks, and a screenings write the store found identical wrote nothing — each is
   *  a guard working, not churn. */
  def isWork(family: String, labels: Labels): Boolean = family match {
    case "kinowo_worker_tasks_enqueued"   =>
      labels.get("result") == "added" && !StampedTaskTypes.contains(labels.get("task_type"))
    case "kinowo_worker_screenings_writes" => labels.get("outcome") == "written"
    case _                                => true
  }

  /** Every axis of `w`: its metrics registry, its re-asks, every write to the corpus, and —
   *  where the wiring counts them — every side-collection write that reached the store.
   *
   *  Corpus writes are counted off `oplog` when the storage is Mongo, and off the change
   *  streams when it is in memory. Not both ways for both: an in-memory store rings its
   *  listeners synchronously, inside the write, so a delivery count is exact; Mongo delivers
   *  on the cursor's own thread whenever it gets there, so a count taken when the pass
   *  returns can miss the pass's own writes or catch the previous pass's. Build it AFTER the
   *  scenario and BEFORE the pass. */
  def ledger(w: TestWiring, oplog: Option[OplogWrites] = None): ChurnLedger = {
    val ledger = new ChurnLedger()
      .registry(w.workerMetrics.registry, WorkFamilies, isWork)
      // The stamped task types `isWork` leaves out of `tasks_enqueued` are counted HERE, or
      // nowhere: without this probe a producer that went round its due gate was invisible.
      .counters(() => w.reaskCountingQueue.reasked.map { case (t, n) => s"re-asked ${t.name}" -> n.toDouble })
      .explain(w.reaskCountingQueue.reaskedKeys.mkString("re-asked keys: ", ", ", ""))
    oplog match {
      case Some(writes) => ledger.counter("corpus writes (oplog)")(writes.count())
      case None =>
        ledger
          .deliveries("movies change stream")(tick => w.movieRepository.watchChanges(_ => tick(), _ => tick()))
          .deliveries("screenings change ring")(tick => w.screeningsRepository.watchApplied((_, ack) => { tick(); ack() }))
          .deliveries("movie_slots change ring")(tick => w.slotsRepository.watchApplied((_, ack) => { tick(); ack() }))
    }
    w.screeningsRepository match {
      case counting: CountingScreeningsRepository => ledger.counter("screenings writes")(counting.writes.get.toLong)
      case _                                      => ()
    }
    w.slotsRepository match {
      case counting: CountingSlotsRepository => ledger.counter("movie_slots writes")(counting.writes.get.toLong)
      case _                                 => ()
    }
    ledger
  }

  /** Subscribe the read-model projector to the `movies` change stream, as production does.
   *  A harness that projects only by `reconcile()` sees every row re-projected whether or
   *  not it changed; attached, a projection happens only when a write asked for one — so a
   *  projection during a no-op pass is itself the signal. */
  def attachProjector(w: TestWiring): Unit = {
    w.movieRepository.watchChanges(w.readModelProjector.onMovieUpsert, w.readModelProjector.onMovieDelete)
    ()
  }

  /** One production tick over whatever the wiring's scrapers report. */
  def run(w: TestWiring): Unit = {
    w.runOneScrapeTick()
    w.drainServices()
    w.drainStaging()
    w.movieService.settle()
    w.movieCache.canonicalizeBySanitize()
    // The detail tasks the settle enqueued, worked as the TaskWorker would work them —
    // the harness's rating drain below completes any task it has no handler for, so
    // leaving them queued would drop them unworked and re-ask them on every pass.
    w.enrichDetailsSync()
    w.readModelProjector.pruneOrphans()
    services.tasks.ReaperSweeps.unresolvedTmdbPeriod(w.unresolvedTmdbReaper, w.clock.instant())
    w.drainServices()
    w.enrichRatingsSync()
  }

  /** Wait until Mongo's change streams have delivered and applied what the last pass wrote:
   *  no event waiting on any cursor's apply thread, and none delivered for `quiet`.
   *
   *  A Mongo cursor delivers whenever it gets there, so without this the projector's work
   *  for one pass lands in the NEXT one's ledger — on 2026-09-24 the UK leg's first pass
   *  merged two films away, their deletes reached the projector during the ledgered pass,
   *  and a pass that wrote nothing to the corpus was reported as retiring a card and
   *  deleting 25 screenings. Call it after the pass that may look and inside the ledgered
   *  body, after the pass. An in-memory store rings its listeners inside the write and
   *  needs neither. `now` is the liveness's own clock, the one its delivery stamps come from. */
  def awaitStreamsQuiet(w: TestWiring, quiet: FiniteDuration = 2.seconds, within: FiniteDuration = 2.minutes): Unit = {
    val liveness = w.movieRepository.changeStreamLiveness
    val watched  = services.movies.ChangeStreamLiveness.Collections.filter(liveness.isWatching)
    val deadline = System.nanoTime() + within.toNanos
    def settled: Boolean = {
      val now = liveness.now()
      watched.forall { c =>
        liveness.pendingApplies(c) == 0 &&
          liveness.lastDelivered(c).forall(at => java.time.Duration.between(at, now).toMillis >= quiet.toMillis)
      }
    }
    while (!settled) {
      if (System.nanoTime() > deadline)
        throw new IllegalStateException(s"the change streams (${watched.mkString(", ")}) were still delivering or applying " +
          s"$within after the pass — a pass that never stops writing is churn in its own right")
      Thread.sleep(100)
    }
  }
}
