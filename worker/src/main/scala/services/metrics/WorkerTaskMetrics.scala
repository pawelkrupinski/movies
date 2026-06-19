package services.metrics

import io.prometheus.metrics.core.metrics.{Counter, Gauge, Histogram}
import io.prometheus.metrics.expositionformats.PrometheusTextFormatWriter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import services.staging.StagingStep
import services.tasks.{QueueSnapshot, Task, TaskState, TaskType}

import java.io.ByteArrayOutputStream
import java.time.Instant

/** Lifecycle hook the [[services.tasks.TaskWorker]] calls so instrumentation
 *  stays out of the worker's hot path. Segregated to the two moments the worker
 *  knows about — a claim, and the outcome of running the handler — so a metrics
 *  sink (or a test spy) implements just these, not the whole queue contract. */
trait TaskObserver {
  /** A worker claimed `task` and is about to run its handler. */
  def onStarted(task: Task): Unit
  /** The handler returned: `outcome` is one of [[WorkerTaskMetrics.Outcomes]];
   *  `handleMillis` is the handler's wall-clock (0 for `no_handler`, which never
   *  ran one). */
  def onFinished(task: Task, outcome: String, handleMillis: Long): Unit
}

object TaskObserver {
  val NoOp: TaskObserver = new TaskObserver {
    def onStarted(task: Task): Unit                                       = ()
    def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = ()
  }
}

/**
 * Prometheus instrumentation for the worker's task pipeline — the worker-side
 * counterpart to the web app's uptime `/metrics` (controllers.MetricsController).
 *
 * It counts the full lifecycle of every deferred task (enqueue → claim → finish)
 * keyed by `TaskType`, times the work, and (refreshed from a live queue snapshot
 * at scrape time) exposes the current backlog. Built on the official Prometheus
 * Java client rather than a hand-rolled exposition; Fly's managed Prometheus
 * scrapes it via the `[[metrics]]` block in fly.worker.toml, and the self-hosted
 * Grafana charts the `rate(...)` of each family per `task_type`.
 *
 * Counters are monotonic since-boot totals — `rate()` handles the reset on each
 * worker reboot. Every `TaskType` (× every label value) is materialized to 0 at
 * construction so its series exists from boot and Grafana draws a continuous
 * line rather than a gap for a type/outcome that hasn't happened yet.
 *
 * Wiring: [[MeteredTaskQueue]] feeds `recordEnqueue`; [[services.tasks.TaskWorker]]
 * feeds `onStarted`/`onFinished` (this class is its `TaskObserver`); the queue
 * gauges are refreshed from a `QueueSnapshot` each `scrape()`.
 */
class WorkerTaskMetrics(poolSize: Int, registry: PrometheusRegistry = new PrometheusRegistry()) extends TaskObserver {
  import WorkerTaskMetrics._

  // The client auto-appends `_total` to counter names, so they're declared without it.
  private val enqueued = Counter.builder()
    .name("kinowo_worker_tasks_enqueued")
    .help("Tasks added to the queue (result=added) or collapsed onto an active duplicate (result=deduped) since boot, by type.")
    .labelNames("task_type", "result")
    .register(registry)

  private val started = Counter.builder()
    .name("kinowo_worker_tasks_started")
    .help("Tasks claimed and kicked off since boot, by type.")
    .labelNames("task_type")
    .register(registry)

  private val finished = Counter.builder()
    .name("kinowo_worker_tasks_finished")
    .help("Tasks that finished a handler run since boot, by type and outcome (done=fully worked, skipped=data already fresh, rescheduled=transient retry, failed=handler threw, no_handler=no wired handler).")
    .labelNames("task_type", "outcome")
    .register(registry)

  private val duration = Histogram.builder()
    .name("kinowo_worker_task_duration_seconds")
    .help("Handler wall-clock of fully-worked (done) tasks, by type.")
    .labelNames("task_type")
    .classicUpperBounds(DurationBucketsSeconds*)
    .classicOnly()
    .register(registry)

  private val queueDepth = Gauge.builder()
    .name("kinowo_worker_queue_depth")
    .help("Tasks currently in the queue, by state.")
    .labelNames("state")
    .register(registry)

  private val waitingByType = Gauge.builder()
    .name("kinowo_worker_queue_waiting_by_type")
    .help("Waiting (claimable) tasks currently in the queue, by type. Sampled from the bounded active snapshot.")
    .labelNames("task_type")
    .register(registry)

  private val oldestWaitingAge = Gauge.builder()
    .name("kinowo_worker_queue_oldest_waiting_age_seconds")
    .help("Age of the oldest waiting task per type — head-of-line latency / starvation signal.")
    .labelNames("task_type")
    .register(registry)

  private val poolSizeGauge = Gauge.builder()
    .name("kinowo_worker_pool_size")
    .help("Configured worker pool size — pair with queue_depth{state=\"worked_on\"} for utilization.")
    .register(registry)

  private val stagingMovies = Gauge.builder()
    .name("kinowo_worker_staging_movies")
    .help("Incubating films currently in pending_movies, by the step each needs next (detail → resolve_tmdb → resolve_imdb → fold). Distinct films (a film's cinema rows count once); sum = total movies in staging.")
    .labelNames("step")
    .register(registry)

  private val writer = PrometheusTextFormatWriter.create()

  seed()

  /** Materialize every series at 0 so it exists from boot (no Grafana gaps). */
  private def seed(): Unit = {
    TaskType.all.foreach { t =>
      EnqueueResults.foreach(r => enqueued.labelValues(t.name, r))
      started.labelValues(t.name)
      Outcomes.foreach(o => finished.labelValues(t.name, o))
      duration.labelValues(t.name)
      waitingByType.labelValues(t.name).set(0.0)
      oldestWaitingAge.labelValues(t.name).set(0.0)
    }
    QueueStates.foreach(s => queueDepth.labelValues(s).set(0.0))
    StagingStep.all.foreach(s => stagingMovies.labelValues(s.label).set(0.0))
    poolSizeGauge.set(poolSize.toDouble)
  }

  def recordEnqueue(taskType: TaskType, result: String): Unit =
    enqueued.labelValues(taskType.name, result).inc()

  def onStarted(task: Task): Unit =
    started.labelValues(task.taskType.name).inc()

  def onFinished(task: Task, outcome: String, handleMillis: Long): Unit = {
    finished.labelValues(task.taskType.name, outcome).inc()
    // Only fully-worked tasks contribute to the duration histogram — a Skipped
    // task only paid a freshness check, and a Reschedule/failure didn't finish.
    if (outcome == Outcome.Done) duration.labelValues(task.taskType.name).observe(handleMillis / 1000.0)
  }

  /** Refresh the queue + staging gauges from live samples and render the full
   *  exposition. Called from the worker's `/metrics` handler on each Fly scrape;
   *  `stagingByStep` comes from `StagingReaper.stepCounts()`. */
  def scrape(snapshot: QueueSnapshot, stagingByStep: Map[StagingStep, Int], now: Instant): String = {
    refreshQueueGauges(snapshot, now)
    StagingStep.all.foreach(s => stagingMovies.labelValues(s.label).set(stagingByStep.getOrElse(s, 0).toDouble))
    val out = new ByteArrayOutputStream()
    writer.write(out, registry.scrape())
    out.toString("UTF-8")
  }

  private def refreshQueueGauges(snapshot: QueueSnapshot, now: Instant): Unit = {
    queueDepth.labelValues(TaskState.Waiting).set(snapshot.counts.getOrElse(TaskState.Waiting, 0L).toDouble)
    queueDepth.labelValues(TaskState.WorkedOn).set(snapshot.counts.getOrElse(TaskState.WorkedOn, 0L).toDouble)

    val waiting = snapshot.active.filter(_.state == TaskState.Waiting).groupBy(_.taskType)
    TaskType.all.foreach { t =>
      val rows = waiting.getOrElse(t.name, Nil)
      waitingByType.labelValues(t.name).set(rows.size.toDouble)
      val age = rows.map(_.submittedAt).minOption
        .map(oldest => math.max(0L, now.getEpochSecond - oldest.getEpochSecond).toDouble)
        .getOrElse(0.0)
      oldestWaitingAge.labelValues(t.name).set(age)
    }
  }
}

object WorkerTaskMetrics {
  object Outcome {
    val Done        = "done"
    val Skipped     = "skipped"
    val Rescheduled = "rescheduled"
    val Failed      = "failed"
    val NoHandler   = "no_handler"
  }
  val Outcomes: Seq[String] =
    Seq(Outcome.Done, Outcome.Skipped, Outcome.Rescheduled, Outcome.Failed, Outcome.NoHandler)

  object EnqueueResult { val Added = "added"; val Deduped = "deduped" }
  val EnqueueResults: Seq[String] = Seq(EnqueueResult.Added, EnqueueResult.Deduped)

  private val QueueStates: Seq[String] = Seq(TaskState.Waiting, TaskState.WorkedOn)

  /** Fixed histogram upper bounds (seconds), spanning a sub-second freshness
   *  skip up to a slow multi-minute scrape/detail fetch. */
  val DurationBucketsSeconds: Seq[Double] = Seq(0.1, 0.5, 1.0, 2.0, 5.0, 10.0, 30.0, 60.0, 120.0)
}
