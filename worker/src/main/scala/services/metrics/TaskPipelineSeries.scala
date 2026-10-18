package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge, Histogram}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.tasks.{QueueSnapshot, Task, TaskState, TaskType}

import java.time.Instant

/** The task pipeline's series: what is enqueued, started and finished, how long a worked task ran, the
 *  queue's depth and held-back work, the first-rating delay, and the chunk page memo. Every series is
 *  seeded to 0 for each country at construction. */
final class TaskPipelineSeries(poolSize: Int, countryCodes: Seq[String], registry: PrometheusRegistry) {

  // The client auto-appends `_total` to counter names, so they're declared without it.
  private val enqueued = Counter.builder()
    .name("kinowo_worker_tasks_enqueued")
    .help("Tasks added to the queue (result=added) or collapsed onto an active duplicate (result=deduped) since boot, by country and type.")
    .labelNames("country", "task_type", "result")
    .register(registry)

  private val started = Counter.builder()
    .name("kinowo_worker_tasks_started")
    .help("Tasks claimed and kicked off since boot, by country and type.")
    .labelNames("country", "task_type")
    .register(registry)

  private val finished = Counter.builder()
    .name("kinowo_worker_tasks_finished")
    .help("Tasks that finished a handler run since boot, by country, type and outcome (done=fully worked, skipped=data already fresh, rescheduled=transient retry, deferred=refused untried by an open circuit, failed=handler threw, no_handler=no wired handler).")
    .labelNames("country", "task_type", "outcome")
    .register(registry)

  private val duration = Histogram.builder()
    .name("kinowo_worker_task_duration_seconds")
    .help("Handler wall-clock of fully-worked (done) tasks, by country and type.")
    .labelNames("country", "task_type")
    .classicUpperBounds(WorkerTaskMetrics.DurationBucketsSeconds*)
    .classicOnly()
    .register(registry)

  private val ratingFirstAttemptDelay = Histogram.builder()
    .name("kinowo_worker_rating_first_attempt_delay_seconds")
    .help("Delay between a film's TMDB resolution and the FIRST attempt to fetch its rating, by country and site (imdb/fw/rt/mc). Measures the EnrichmentReaper's first-pass latency now that ratings aren't enqueued the instant a film resolves.")
    .labelNames("country", "site")
    .classicUpperBounds(WorkerTaskMetrics.RatingDelayBucketsSeconds*)
    .classicOnly()
    .register(registry)

  private val queueDepth = Gauge.builder()
    .name("kinowo_worker_queue_depth")
    .help("Tasks currently in the queue, by country and state.")
    .labelNames("country", "state")
    .register(registry)

  private val waitingByType = Gauge.builder()
    .name("kinowo_worker_queue_waiting_by_type")
    .help("Waiting tasks currently in the queue, by country and type — claimable AND held back (see kinowo_worker_queue_held_by_type; claimable = waiting - held). Sampled from the bounded active snapshot.")
    .labelNames("country", "task_type")
    .register(registry)

  // Most of the waiting set is often work the queue is holding back on purpose — a chunked
  // venue's whole staggered fan-out counts as waiting the instant it is planned. Without
  // this split a planned burst reads as a backlog: 2026-09-21..28 held chunks were 75-95%
  // of every waiting spike, with the pool mostly idle through them.
  private val heldByType = Gauge.builder()
    .name("kinowo_worker_queue_held_by_type")
    .help("Waiting tasks the queue is holding back (retry backoff, a Deferred's instant, a staggered chunk not yet due), by country and type — the not-yet-claimable part of kinowo_worker_queue_waiting_by_type. Sampled from the bounded active snapshot.")
    .labelNames("country", "task_type")
    .register(registry)

  private val oldestWaitingAge = Gauge.builder()
    .name("kinowo_worker_queue_oldest_waiting_age_seconds")
    .help("Seconds the oldest CLAIMABLE waiting task per country and type has been claimable (tasks held in retry backoff or not yet due are excluded) — head-of-line latency / starvation signal.")
    .labelNames("country", "task_type")
    .register(registry)

  // The mirror of the head-of-line age: that one counts only CLAIMABLE rows, so a task the
  // queue holds back is invisible to it by design. This is the longest remaining hold among
  // the held-back rows — never more than TaskWorker.MaxBackoff unless something parked a
  // task past the cap (alerted as WorkerTaskParkedTooLong).
  private val parkedMax = Gauge.builder()
    .name("kinowo_worker_queue_parked_max_seconds")
    .help("Longest remaining hold, in seconds, among waiting tasks per country and type that the queue is holding back (retry backoff, a Deferred's instant, a staggered chunk not yet due); 0 when none is held. Bounded by TaskWorker.MaxBackoff (1800s) for every hold the queue sets, so a value above it means a task parked past the cap. Sampled from the bounded active snapshot.")
    .labelNames("country", "task_type")
    .register(registry)

  // The worker pool is a single SharedExecutionBudget across all countries, so
  // this is a process-level gauge with no country label (pairing it per-country
  // would misleadingly imply a per-country pool).
  private val poolSizeGauge = Gauge.builder()
    .name("kinowo_worker_pool_size")
    .help("Configured worker pool size (shared across countries) — pair with queue_depth{state=\"worked_on\"} for utilization.")
    .register(registry)

  private val chunkPages = Counter.builder()
    .name("kinowo_worker_chunk_page_memo")
    .help("Page-at-a-time scrape chunks (Flicks day pages) since boot, by country and outcome: hit = the page was byte-identical to the one this chunk last parsed, so that parse was reused and the page not parsed; changed = the page differed and was parsed; new = no earlier parse was remembered; parser = the page was the same but the parser has changed since. hit/(all) is the share of page parsing spared.")
    .labelNames("country", "outcome")
    .register(registry)

  countryCodes.foreach { c =>
    TaskType.all.foreach { t =>
      WorkerTaskMetrics.EnqueueResults.foreach(r => enqueued.labelValues(c, t.name, r))
      started.labelValues(c, t.name)
      WorkerTaskMetrics.Outcomes.foreach(o => finished.labelValues(c, t.name, o))
      duration.labelValues(c, t.name)
      waitingByType.labelValues(c, t.name).set(0.0)
      heldByType.labelValues(c, t.name).set(0.0)
      oldestWaitingAge.labelValues(c, t.name).set(0.0)
      parkedMax.labelValues(c, t.name).set(0.0)
    }
    WorkerTaskMetrics.RatingSites.foreach(s => ratingFirstAttemptDelay.labelValues(c, s))
    WorkerTaskMetrics.QueueStates.foreach(s => queueDepth.labelValues(c, s).set(0.0))
    services.tasks.ChunkPageMemoMetrics.Outcomes.foreach(o => chunkPages.labelValues(c, o))
  }
  poolSizeGauge.set(poolSize.toDouble)

  def recordEnqueue(country: String, taskType: TaskType, result: String): Unit =
    enqueued.labelValues(country, taskType.name, result).inc()

  def onStarted(country: String, task: Task): Unit =
    started.labelValues(country, task.taskType.name).inc()

  def onFinished(country: String, task: Task, outcome: String, handleMillis: Long): Unit = {
    finished.labelValues(country, task.taskType.name, outcome).inc()
    // Only fully-worked tasks contribute to the duration histogram — a Skipped
    // task only paid a freshness check, and a Reschedule/failure didn't finish.
    if (outcome == WorkerTaskMetrics.Outcome.Done) duration.labelValues(country, task.taskType.name).observe(handleMillis / 1000.0)
  }

  def recordFirstRatingDelay(country: String, site: String, seconds: Double): Unit =
    ratingFirstAttemptDelay.labelValues(country, site).observe(math.max(0.0, seconds))

  def recordChunkPage(country: String, outcome: String): Unit               = chunkPages.labelValues(country, outcome).inc()

  /** Refresh `country`'s queue gauges from its live snapshot. */
  def refreshQueueGauges(country: String, snapshot: QueueSnapshot, now: Instant): Unit = {
    queueDepth.labelValues(country, TaskState.Waiting).set(snapshot.counts.getOrElse(TaskState.Waiting, 0L).toDouble)
    queueDepth.labelValues(country, TaskState.WorkedOn).set(snapshot.counts.getOrElse(TaskState.WorkedOn, 0L).toDouble)

    val waiting = snapshot.active.filter(_.state == TaskState.Waiting).groupBy(_.taskType)
    TaskType.all.foreach { t =>
      val rows = waiting.getOrElse(t.name, Nil)
      waitingByType.labelValues(country, t.name).set(rows.size.toDouble)
      // Head-of-line age counts only CLAIMABLE rows, from when each became claimable:
      // a row parked in retry backoff (or a staggered chunk not yet due) is being held
      // back on purpose, and aging it from `submittedAt` made one retrying task read as
      // a pool that could not keep up (UK/ES 2026-09-18..22, pool idle throughout).
      val age = rows.filter(_.claimableAt(now)).map(_.claimableSince).minOption
        .map(oldest => math.max(0L, now.getEpochSecond - oldest.getEpochSecond).toDouble)
        .getOrElse(0.0)
      oldestWaitingAge.labelValues(country, t.name).set(age)
      val holds  = rows.flatMap(_.nextEligibleAt).filter(_.isAfter(now))
      heldByType.labelValues(country, t.name).set(holds.size.toDouble)
      val parked = holds.map(until => until.getEpochSecond - now.getEpochSecond).maxOption.getOrElse(0L)
      parkedMax.labelValues(country, t.name).set(parked.toDouble)
    }
  }
}