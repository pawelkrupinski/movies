package modules.wiring

import modules.WorkerWiring
import services.events.TaskFinished
import services.freshness.{FreshnessStore, MongoFreshnessStore}
import services.metrics.MeteredTaskQueue
import services.schedule.{AlwaysClaimScheduledRunStore, MongoScheduledRunStore, ScheduledRunStore}
import services.tasks.{CachingTaskQueue, LivenessWatchdog, MongoTaskQueue, TaskQueue, TaskWorker, WorkerHeartbeat}
import tools.Env

import scala.concurrent.duration.DurationLong

/** The durable task queue every scrape / enrichment rides, the freshness stamps
 *  and cluster-wide run claims the reapers schedule from, and the fixed pool
 *  that drains the queue — with its heartbeat and wedged-JVM watchdog. */
trait TaskQueueWiring { self: WorkerWiring =>

  // Metered (counts every enqueue attempt, incl. cache-served duplicates) wraps the
  // local dedup cache (skips the redundant enqueue round-trip) wraps Mongo.
  // Held so its occupancy can be published (`kinowo_worker_cache_*`); the queue
  // itself is consumed through the `TaskQueue` abstraction below.
  lazy val taskDedupCache: CachingTaskQueue = new CachingTaskQueue(new MongoTaskQueue(mongoConnection.database))
  lazy val taskQueue: TaskQueue = new MeteredTaskQueue(taskDedupCache, taskMetrics)
  lazy val freshnessStore: FreshnessStore = new MongoFreshnessStore(mongoConnection.database)

  // THE REAPER-BACKOFF THROTTLE IS GONE, and the shape of what it was is worth a
  // paragraph because the enqueue caps it shared are still here.
  //
  // It let something outside the worker say "you are CPU-starved, ease off": the
  // reapers trimmed enqueue and the pool duty-cycled, so the box earned idle. Its
  // producer was Fly's shared-CPU credit balance — an in-process poller against
  // `api.fly.io`, backstopped by a Grafana webhook POSTing to
  // `kinowo-worker*.internal:9000/throttle` over 6PN. Credit is a Fly billing concept
  // with no counterpart for a pod on a dedicated eight-core box, and the poller had
  // been failing open since the org's read-only tokens were revoked, so by the time
  // the platform went the whole path had been inert for weeks. It was deleted rather
  // than re-pointed at a k3s signal because nothing had asked it to do anything in
  // that time. `ScrapeCadence.MaxEnqueuePerTick` / `MaxOutstandingScrapeTasks` and
  // `ScrapeReaper`'s boot ramp and spread survive it: those smooth the load the
  // reaper itself creates and never needed an external opinion.
  //
  // A restart primitive went with it. A sustained credit floor used to exit non-zero
  // so Fly rescheduled the machine; that was dropped 2026-07-03 when the floor turned
  // out to be STRUCTURAL (steady CPU just over the earn rate), making the restart a
  // self-inflicted ~45-min loop that PREVENTED the recovery it meant to force —
  // measured 4 restarts/3h while the box was near-idle. After that both throttle paths
  // only alarmed, and with the paths gone the primitive had no caller at all.

  // Cluster-wide occurrence claims gate the reapers' recurring ticks so each
  // scheduled occurrence runs on ONE machine (rotating), not on every machine.
  // Absent Mongo (local dev opt-out) → always-claim, i.e. run unlocked.
  lazy val scheduledRunStore: ScheduledRunStore =
    mongoConnection.database
      .map(db => new MongoScheduledRunStore(db.getCollection[org.mongodb.scala.bson.collection.immutable.Document]("scheduled_runs")))
      .getOrElse(AlwaysClaimScheduledRunStore)

  // A fixed pool of workers, each fetching and running ONE task at a time — so
  // the number of scrapes/enrichments in flight at once is hard-capped at the
  // pool size and a backlog can't peg the box. (Replaces the old single batch
  // poller that claimed up to 20 tasks per tick onto a shared-budget EC.)
  def workerPoolSize: Int = Env.positiveInt("KINOWO_WORKER_POOL_SIZE", 4)
  lazy val taskWorker = new TaskWorker(
    taskQueue, Seq(scrapeCinemaHandler, enrichDetailsHandler, scrapeChunkHandler, scrapeChunkReduceHandler) ++ ratingHandlers ++ operatorHandlers ++ stagingHandlers,
    poolSize = workerPoolSize,
    // The SAME composite credit-throttle signal the reapers read, so the pool
    // duty-cycles in lockstep with the enqueue-backoff under a credit crunch.
    // Each completed task announces itself so StagingReaper can chain the next
    // staging step; non-staging completions are ignored by its subscriber.
    onCompleted = task => eventBus.publish(TaskFinished(task.taskType, task.dedupKey, task.payload)),
    // Report claims / outcomes / handler durations to the Prometheus metrics.
    observer = taskMetrics
  )
  // Logs queue depth every minute so a CPU-credit/steal episode can be correlated
  // with the scrape/enrich backlog that drove it (the diagnostic that was missing
  // when the 2026-06-12 worker-steal episode had to be reconstructed from metrics).
  lazy val workerHeartbeat = new WorkerHeartbeat(taskQueue)

  // Last-resort backstop for a WEDGED-but-alive JVM (the 2026-06-23 heap OOM, where
  // the process limped on for ~2h answering /health 200 because the OOM had killed
  // its worker threads but not the process, and the throttle watchdog couldn't see
  // it — the credit poller failed open to "healthy"). The LivenessWatchdog watches
  // the heartbeat pulse and, if it stalls past the threshold, dumps the about-to-die
  // heap to the Fly volume (so a leak-vs-too-tight analysis is possible offline) and
  // exits non-zero so Fly reschedules. Threshold sits several heartbeat intervals
  // above the 1-min pulse so GC jitter never trips it.
  def livenessStaleMinutes: Long = Env.positiveLong("KINOWO_WORKER_LIVENESS_STALE_MINUTES", 5L)
  def heapDumpDir: String        = Env.get("KINOWO_HEAP_DUMP_DIR").getOrElse("/data/heapdumps")
  lazy val livenessWatchdog = new LivenessWatchdog(
    lastBeatMillis     = () => workerHeartbeat.lastTickMillis,
    stalenessThreshold = livenessStaleMinutes.minutes,
    onWedged           = () => { tools.HeapDumper.dump(heapDumpDir); sys.exit(70) })
}
