package services.metrics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.WorkerTaskMetrics.CountryQueueSample
import services.movies.{ChangeStreamLiveness, MergeReason}
import services.staging.StagingStep
import services.tasks.{QueueSnapshot, Task, TaskState, TaskSummary, TaskType}

import java.time.Instant

/**
 * Locks the worker's Prometheus task-pipeline exposition: the four lifecycle
 * counters/histogram (enqueued → started → finished + handler duration) and the
 * queue gauges refreshed from a live snapshot. The histogram's "fully-worked
 * only" rule and the per-type seeding (every series exists from boot) are the
 * load-bearing behaviour, so they're asserted directly.
 *
 * Multi-country: one JVM runs a wiring per country against a single shared
 * `Series`; every series carries a leading `country` label so the countries never
 * collide on the one `/metrics` endpoint. The country-label presence + isolation
 * are asserted explicitly below.
 */
class WorkerTaskMetricsSpec extends AnyFlatSpec with Matchers {

  private val now = Instant.parse("2026-06-19T10:00:00Z")

  private def task(t: TaskType) = Task("id", t, "dedup", Map.empty, attempts = 1)

  private def summary(taskType: TaskType, state: String, submittedAt: Instant, nextEligibleAt: Option[Instant] = None) =
    TaskSummary("id", taskType.name, "dedup", state, submittedAt, attempts = 1,
      workerId = None, leaseExpiresAt = None, lastError = None, nextEligibleAt = nextEligibleAt)

  private val emptySnapshot = QueueSnapshot(Map.empty, Nil)
  private val noStaging      = Map.empty[StagingStep, Int]

  /** A single-country ("pl") series + its facade — the common case. */
  private def newPl(): (WorkerTaskMetrics, WorkerTaskMetrics.Series) = {
    val series = new WorkerTaskMetrics.Series(poolSize = 4, countryCodes = Seq("pl"))
    (new WorkerTaskMetrics("pl", series), series)
  }

  private def scrapePl(series: WorkerTaskMetrics.Series,
                       snapshot: QueueSnapshot = emptySnapshot,
                       staging: Map[StagingStep, Int] = noStaging,
                      ): String =
    series.scrape(Seq(CountryQueueSample("pl", snapshot, staging, ChangeStreamLiveness.unwatched())), now)

  it should "tag every task-pipeline series with the emitting country" in {
    val (m, series) = newPl()
    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    m.onStarted(task(TaskType.ResolveTmdb))

    val out = scrapePl(series)

    // The label is present and carries "pl" on representative counter + gauge series.
    out should include ("""kinowo_worker_tasks_enqueued_total{country="pl",result="added",task_type="ScrapeCinema"} 1""")
    out should include ("""kinowo_worker_tasks_started_total{country="pl",task_type="ResolveTmdb"} 1""")
  }

  it should "keep two countries' series separate on one shared registry" in {
    val series = new WorkerTaskMetrics.Series(poolSize = 4, countryCodes = Seq("pl", "uk"))
    val pl = new WorkerTaskMetrics("pl", series)
    val uk = new WorkerTaskMetrics("uk", series)

    pl.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    pl.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    uk.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)

    val out = series.scrape(Seq(
      CountryQueueSample("pl", emptySnapshot, noStaging, ChangeStreamLiveness.unwatched()),
      CountryQueueSample("uk", emptySnapshot, noStaging, ChangeStreamLiveness.unwatched())), now)

    out should include ("""kinowo_worker_tasks_enqueued_total{country="pl",result="added",task_type="ScrapeCinema"} 2""")
    out should include ("""kinowo_worker_tasks_enqueued_total{country="uk",result="added",task_type="ScrapeCinema"} 1""")
    // Per-country throttle: pl backing off, uk not.
  }

  // THE SILENT CURSOR. The change-event counters read the same for a stalled cursor and a
  // quiet night; the age of the last delivered event is what separates them, and it must be
  // recomputed on every scrape so that a stall draws a diagonal rather than a frozen value.
  it should "age every change-stream cursor off its last delivered event, at scrape time" in {
    val (_, series) = newPl()
    val clock    = new tools.MutableClock(now.minusSeconds(600))
    val liveness = new ChangeStreamLiveness(clock)                  // "booted" 600s before `now`
    clock.advanceSeconds(480)
    liveness.delivered(ChangeStreamLiveness.Movies)                 // the movies cursor delivered 120s before `now`

    val out = series.scrape(Seq(CountryQueueSample("pl", emptySnapshot, noStaging, liveness)), now)
    out should include ("""kinowo_worker_change_stream_last_event_age_seconds{collection="movies",country="pl"} 120.0""")
    // A cursor that never delivered ages from the boot, not from zero — "never" is the loudest silence.
    out should include ("""kinowo_worker_change_stream_last_event_age_seconds{collection="movie_slots",country="pl"} 600.0""")
    out should include ("""kinowo_worker_change_stream_last_event_age_seconds{collection="screenings",country="pl"} 600.0""")

    // Nothing delivered since: the next scrape reads a LARGER age, not the same one.
    val later = series.scrape(Seq(CountryQueueSample("pl", emptySnapshot, noStaging, liveness)), now.plusSeconds(60))
    later should include ("""kinowo_worker_change_stream_last_event_age_seconds{collection="movies",country="pl"} 180.0""")
  }

  // A cursor that delivers on time can still sit behind a stuck apply thread — the half the
  // delivery age cannot see. Queued-but-unapplied per cursor, and the oldest one's wait, both
  // recomputed per scrape so a stuck apply climbs instead of freezing.
  it should "publish each cursor's unapplied backlog and the oldest one's wait, at scrape time" in {
    val (_, series) = newPl()
    val clock    = new tools.MutableClock(now.minusSeconds(300))
    val liveness = new ChangeStreamLiveness(clock)
    liveness.queued(ChangeStreamLiveness.Screenings)                // handed off 300s before `now`
    clock.advanceSeconds(200)
    val done = liveness.queued(ChangeStreamLiveness.Screenings)
    liveness.queued(ChangeStreamLiveness.Screenings)
    liveness.applied(ChangeStreamLiveness.Screenings, done)

    val out = series.scrape(Seq(CountryQueueSample("pl", emptySnapshot, noStaging, liveness)), now)
    out should include ("""kinowo_worker_change_stream_apply_pending{collection="screenings",country="pl"} 2.0""")
    out should include ("""kinowo_worker_change_stream_apply_lag_seconds{collection="screenings",country="pl"} 300.0""")
    out should include ("""kinowo_worker_change_stream_apply_pending{collection="movies",country="pl"} 0.0""")
    out should include ("""kinowo_worker_change_stream_apply_lag_seconds{collection="movies",country="pl"} 0.0""")

    val later = series.scrape(Seq(CountryQueueSample("pl", emptySnapshot, noStaging, liveness)), now.plusSeconds(60))
    later should include ("""kinowo_worker_change_stream_apply_lag_seconds{collection="screenings",country="pl"} 360.0""")
  }

  it should "count re-try resolves that landed on a queued one by mode and whether they upgraded it" in {
    val (pl, series) = newPl()
    pl.recordDuplicate(services.tasks.ResolveMode.RetryMiss, upgraded = true)
    pl.recordDuplicate(services.tasks.ResolveMode.Force, upgraded = false)

    val out = series.scrape(Seq(CountryQueueSample("pl", emptySnapshot, noStaging, ChangeStreamLiveness.unwatched())), now)
    out should include ("""kinowo_worker_resolve_retry_duplicates_total{country="pl",mode="retry-miss",outcome="upgraded"} 1""")
    out should include ("""kinowo_worker_resolve_retry_duplicates_total{country="pl",mode="force",outcome="not-upgraded"} 1""")
    out should include ("""kinowo_worker_resolve_retry_duplicates_total{country="pl",mode="retry-miss",outcome="not-upgraded"} 0""")
  }

  it should "count the rows the read-model sweep re-projected behind a silent change stream" in {
    val (m, series) = newPl()
    m.recordCatchUp(3)
    m.recordCatchUp(0)   // a quiet sweep adds nothing, and the series exists from boot regardless
    scrapePl(series) should include ("""kinowo_worker_readmodel_catchup_rows_total{country="pl"} 3""")
  }

  it should "count newcomer kicks and the staging rows they decoded, from zero at boot" in {
    val (m, series) = newPl()
    scrapePl(series) should include ("""kinowo_worker_staging_newcomer_kicks_total{country="pl"} 0""")
    m.recordNewcomerKick(1)
    m.recordNewcomerKick(3)
    val out = scrapePl(series)
    out should include ("""kinowo_worker_staging_newcomer_kicks_total{country="pl"} 2""")
    out should include ("""kinowo_worker_staging_newcomer_kick_rows_total{country="pl"} 4""")
  }

  "WorkerTaskMetrics" should "count enqueues by type and result" in {
    val (m, series) = newPl()
    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Deduped)

    val out = scrapePl(series)

    out should include ("""kinowo_worker_tasks_enqueued_total{country="pl",result="added",task_type="ScrapeCinema"} 2""")
    out should include ("""kinowo_worker_tasks_enqueued_total{country="pl",result="deduped",task_type="ScrapeCinema"} 1""")
  }

  it should "count started and finished tasks, split by outcome" in {
    val (m, series) = newPl()
    m.onStarted(task(TaskType.ResolveTmdb))
    m.onFinished(task(TaskType.ResolveTmdb), WorkerTaskMetrics.Outcome.Done, handleMillis = 500)
    m.onStarted(task(TaskType.ResolveTmdb))
    m.onFinished(task(TaskType.ResolveTmdb), WorkerTaskMetrics.Outcome.Skipped, handleMillis = 5)

    val out = scrapePl(series)

    out should include ("""kinowo_worker_tasks_started_total{country="pl",task_type="ResolveTmdb"} 2""")
    out should include ("""kinowo_worker_tasks_finished_total{country="pl",outcome="done",task_type="ResolveTmdb"} 1""")
    out should include ("""kinowo_worker_tasks_finished_total{country="pl",outcome="skipped",task_type="ResolveTmdb"} 1""")
  }

  it should "record handler duration ONLY for fully-worked (done) tasks" in {
    val (m, series) = newPl()
    m.onFinished(task(TaskType.EnrichDetails), WorkerTaskMetrics.Outcome.Done, handleMillis = 1500)
    m.onFinished(task(TaskType.EnrichDetails), WorkerTaskMetrics.Outcome.Skipped, handleMillis = 9000)

    val out = scrapePl(series)

    // Only the 1.5s done observation counts — the skipped one is excluded.
    out should include ("""kinowo_worker_task_duration_seconds_count{country="pl",task_type="EnrichDetails"} 1""")
    out should include ("""kinowo_worker_task_duration_seconds_sum{country="pl",task_type="EnrichDetails"} 1.5""")
    // 1.5s falls in the le=2.0 bucket but not le=1.0 (le is the last label, per convention).
    out should include ("""kinowo_worker_task_duration_seconds_bucket{country="pl",task_type="EnrichDetails",le="2.0"} 1""")
    out should include ("""kinowo_worker_task_duration_seconds_bucket{country="pl",task_type="EnrichDetails",le="1.0"} 0""")
  }

  it should "expose queue depth, per-type waiting, and oldest-waiting age from the snapshot" in {
    val (_, series) = newPl()
    val snapshot = QueueSnapshot(
      counts = Map(TaskState.Waiting -> 5L, TaskState.WorkedOn -> 2L),
      active = Seq(
        summary(TaskType.ScrapeCinema, TaskState.Waiting, now.minusSeconds(30)),
        summary(TaskType.ScrapeCinema, TaskState.Waiting, now.minusSeconds(90)),
        summary(TaskType.EnrichDetails, TaskState.WorkedOn, now.minusSeconds(10))
      ))

    val out = scrapePl(series, snapshot)

    out should include ("""kinowo_worker_queue_depth{country="pl",state="waiting"} 5""")
    out should include ("""kinowo_worker_queue_depth{country="pl",state="worked_on"} 2""")
    out should include ("""kinowo_worker_queue_waiting_by_type{country="pl",task_type="ScrapeCinema"} 2""")
    out should include ("""kinowo_worker_queue_oldest_waiting_age_seconds{country="pl",task_type="ScrapeCinema"} 90""")
    // The pool is a single shared budget across countries, so pool_size is unlabelled.
    out should include ("kinowo_worker_pool_size 4")
  }

  // The 2026-09-18..22 `Worker task queue head-of-line age high` episodes (UK ScrapeChunk
  // up to 1601s, ES ResolveTmdb up to 6239s) were ONE task each, sitting out its retry
  // backoff while the pool idled (worked_on 0-3 of 4): the gauge aged tasks from
  // `submittedAt`, so a task the queue was deliberately holding back read as one the
  // pool could not reach. Head-of-line age is how long CLAIMABLE work has waited.
  it should "age the head of line from when a task became claimable, not from when it was submitted" in {
    val (_, series) = newPl()
    val snapshot = QueueSnapshot(
      counts = Map(TaskState.Waiting -> 3L),
      active = Seq(
        // Submitted 25 min ago, parked in backoff until 5 min from now: not claimable, not queued behind anything.
        summary(TaskType.ScrapeChunk, TaskState.Waiting, now.minusSeconds(1500), Some(now.plusSeconds(300))),
        // Submitted 25 min ago, its backoff ran out 40s ago: it has waited on the pool for 40s.
        summary(TaskType.ScrapeChunk, TaskState.Waiting, now.minusSeconds(1500), Some(now.minusSeconds(40))),
        // Only ever parked: nothing claimable of this type at all.
        summary(TaskType.ResolveTmdb, TaskState.Waiting, now.minusSeconds(6000), Some(now.plusSeconds(1800)))
      ))

    val out = scrapePl(series, snapshot)

    out should include ("""kinowo_worker_queue_oldest_waiting_age_seconds{country="pl",task_type="ScrapeChunk"} 40""")
    out should include ("""kinowo_worker_queue_oldest_waiting_age_seconds{country="pl",task_type="ResolveTmdb"} 0""")
  }

  // The head-of-line age leaves held-back tasks out on purpose, so a task parked far past
  // the backoff cap — the "parked in backoff forever" class — was visible on no gauge at all.
  it should "expose the longest remaining hold among held-back tasks, 0 when none is held" in {
    val (_, series) = newPl()
    val snapshot = QueueSnapshot(
      counts = Map(TaskState.Waiting -> 4L),
      active = Seq(
        summary(TaskType.ScrapeChunk, TaskState.Waiting, now.minusSeconds(1500), Some(now.plusSeconds(300))),
        summary(TaskType.ScrapeChunk, TaskState.Waiting, now.minusSeconds(1500), Some(now.plusSeconds(1200))),
        // Its hold has already run out: claimable, not parked.
        summary(TaskType.ResolveTmdb, TaskState.Waiting, now.minusSeconds(6000), Some(now.minusSeconds(40))),
        summary(TaskType.EnrichDetails, TaskState.Waiting, now.minusSeconds(60), None)
      ))

    val out = scrapePl(series, snapshot)

    out should include ("""kinowo_worker_queue_parked_max_seconds{country="pl",task_type="ScrapeChunk"} 1200""")
    out should include ("""kinowo_worker_queue_parked_max_seconds{country="pl",task_type="ResolveTmdb"} 0""")
    out should include ("""kinowo_worker_queue_parked_max_seconds{country="pl",task_type="EnrichDetails"} 0""")
    out should include ("""kinowo_worker_queue_parked_max_seconds{country="pl",task_type="StagingFold"} 0""")
  }

  it should "seed every task type to 0 so the series exists from boot" in {
    val (_, series) = newPl()
    val out = scrapePl(series)

    // A type that never ran still appears at 0 (no Grafana gap).
    out should include ("""kinowo_worker_tasks_started_total{country="pl",task_type="StagingFold"} 0""")
    out should include ("""kinowo_worker_tasks_finished_total{country="pl",outcome="done",task_type="RtRating"} 0""")
  }

  it should "expose staging movie counts by step, seeding unused steps to 0" in {
    val (_, series) = newPl()
    val staging = Map[StagingStep, Int](StagingStep.Detail -> 3, StagingStep.Fold -> 1)

    val out = scrapePl(series, staging = staging)

    out should include ("""kinowo_worker_staging_movies{country="pl",step="detail"} 3""")
    out should include ("""kinowo_worker_staging_movies{country="pl",step="fold"} 1""")
    // A step with nobody waiting still appears at 0.
    out should include ("""kinowo_worker_staging_movies{country="pl",step="resolve_tmdb"} 0""")
    out should include ("""kinowo_worker_staging_movies{country="pl",step="resolve_imdb"} 0""")
  }

  it should "count movie-row merges by reason, summing victims and seeding unused reasons to 0" in {
    val (m, series) = newPl()
    m.recordMerge(MergeReason.Canonicalize, 2)
    m.recordMerge(MergeReason.Canonicalize, 1)
    m.recordMerge(MergeReason.TmdbIdentity, 1)
    m.recordMerge(MergeReason.NormalizeRebuild, 2)
    m.recordMerge(MergeReason.ResolvedSettle, 0)  // a no-victim fold contributes nothing

    val out = scrapePl(series)

    out should include ("""kinowo_worker_merges_total{country="pl",reason="canonicalize"} 3""")
    out should include ("""kinowo_worker_merges_total{country="pl",reason="tmdb-identity"} 1""")
    out should include ("""kinowo_worker_merges_total{country="pl",reason="normalize-rebuild"} 2""")
    // Seeded so the series exists from boot; the 0-victim call left it at 0.
    out should include ("""kinowo_worker_merges_total{country="pl",reason="resolved-settle"} 0""")
  }

  it should "count re-keys per reason and seed every reason to 0" in {
    val (m, series) = newPl()
    m.recordRekey(services.movies.RekeyReason.ResolvedYear)
    m.recordRekey(services.movies.RekeyReason.ResolvedYear)
    m.recordRekey(services.movies.RekeyReason.Canonicalize)

    val out = scrapePl(series)
    out should include ("""kinowo_worker_rekeys_total{country="pl",reason="resolved-year"} 2""")
    out should include ("""kinowo_worker_rekeys_total{country="pl",reason="canonicalize"} 1""")
    out should include ("""kinowo_worker_rekeys_total{country="pl",reason="embedded-year"} 0""")
    out should include ("""kinowo_worker_rekeys_total{country="pl",reason="forced-reset"} 0""")
    out should include ("""kinowo_worker_rekeys_total{country="pl",reason="scrape-variant"} 0""")
  }

  it should "count movie-row splits, summing fragments and seeding the series to 0" in {
    val (m, series) = newPl()
    m.recordSplit(2)  // a settle pass re-diverted two slots
    m.recordSplit(1)
    m.recordSplit(0)  // a pass that found nothing contributes nothing

    scrapePl(series) should include ("""kinowo_worker_splits_total{country="pl"} 3""")
  }

  it should "seed the splits series to 0 so it exists from boot" in {
    val (_, series) = newPl()
    scrapePl(series) should include ("""kinowo_worker_splits_total{country="pl"} 0""")
  }

  it should "observe the TMDB-resolved → first-rating-attempt delay per site, seeding all four" in {
    val (m, series) = newPl()
    m.recordFirstRatingDelay("imdb", 300.0)

    val out = scrapePl(series)

    out should include ("""kinowo_worker_rating_first_attempt_delay_seconds_count{country="pl",site="imdb"} 1""")
    // Seeded so every site's series exists from boot even before its first observation.
    out should include ("""kinowo_worker_rating_first_attempt_delay_seconds_count{country="pl",site="fw"} 0""")
    out should include ("""kinowo_worker_rating_first_attempt_delay_seconds_count{country="pl",site="rt"} 0""")
    out should include ("""kinowo_worker_rating_first_attempt_delay_seconds_count{country="pl",site="mc"} 0""")
  }

  // A counter series that is BORN at 1 is invisible to `increase()`/`rate()` — they need two
  // samples, and the first one already holds the failure. So the incident's own shape, one
  // codec failure on `movies.upsert`, must land on a series that already reads 0.
  it should "seed every known repository write failure at zero, so increase() sees the first one" in {
    val (m, series) = newPl()
    val line = """kinowo_worker_repository_write_failed_total{collection="movies",country="pl",exception="CodecConfigurationException",op="upsert"}"""

    scrapePl(series).linesIterator.find(_.startsWith(line)).map(_.split(' ').last.toDouble) shouldBe Some(0.0)

    m.recordWriteFailed("movies", "upsert", "CodecConfigurationException")
    scrapePl(series).linesIterator.find(_.startsWith(line)).map(_.split(' ').last.toDouble) shouldBe Some(1.0)
  }

  // ONE READ, ONE COUNTRY'S GAUGES. The worker renders its whole exposition — every country, the
  // JVM, every counter in the registry — in one pass, and a render that throws keeps the LAST GOOD
  // BYTES. The queue and staging reads throw on failure (an unreadable queue is not an empty one),
  // so a sample that let them escape froze every series the worker exports for as long as either
  // read kept failing: counters flat, change-stream ages stopped, and no alert able to tell.
  it should "render the rest of the exposition when a country's queue or staging read fails, holding only those gauges" in {
    val (m, series) = newPl()
    val clock    = new tools.MutableClock(now.minusSeconds(10))
    val liveness = new ChangeStreamLiveness(clock)
    liveness.delivered(ChangeStreamLiveness.Movies)                 // 10s before `now`
    val queued   = QueueSnapshot(Map(TaskState.Waiting -> 5L), Nil)
    series.scrape(Seq(CountryQueueSample.read("pl", queued, Map(StagingStep.Detail -> 3), liveness)), now)

    m.recordEnqueue(TaskType.ScrapeCinema, WorkerTaskMetrics.EnqueueResult.Added)
    val out = series.scrape(Seq(CountryQueueSample.read("pl",
      throw new IllegalStateException("queue unreadable"),
      throw new IllegalStateException("staging read incomplete"), liveness)), now.plusSeconds(60))

    // The render went ahead: a counter moved since the last good one shows its new value …
    out should include ("""kinowo_worker_tasks_enqueued_total{country="pl",result="added",task_type="ScrapeCinema"} 1""")
    // … the change-stream age kept climbing …
    out should include ("""kinowo_worker_change_stream_last_event_age_seconds{collection="movies",country="pl"} 70.0""")
    // … and the two gauges whose read failed hold their last reading rather than claim an empty queue.
    out should include ("""kinowo_worker_queue_depth{country="pl",state="waiting"} 5""")
    out should include ("""kinowo_worker_staging_movies{country="pl",step="detail"} 3""")
  }
}
