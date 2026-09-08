package services

import com.mongodb.client.model.UpdateOptions
import java.util.concurrent.TimeUnit
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, SingleObservableFuture}
import org.mongodb.scala.model.{Filters, Indexes, Updates}
import play.api.Logging

import java.util.concurrent.{ConcurrentHashMap, ConcurrentSkipListMap, Executors, ScheduledExecutorService}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * @parameter surfaceExternalWrites when true, after hydrating from Mongo this
 *        instance POLLS `uptimeBuckets` every `PollIntervalMs` and merges the
 *        current snapshot — including buckets written by OTHER processes — into
 *        its in-memory map, firing listeners for the ones that changed so the
 *        live /uptime SSE reflects them. The web (serving) process sets this so
 *        it surfaces the worker's scraper + enrichment metrics; the worker
 *        leaves it off — nothing reads its map.
 *
 * Both ends are BOUNDED, decoupled from the scraper fetch rate:
 *  - WRITES are batched: `recordSuccess`/`recordFailure` only mutate the
 *    in-memory bucket (cheap) and mark it dirty; a daemon flusher writes each
 *    dirty bucket's CUMULATIVE counts via `$set` every `FlushIntervalMs`. A
 *    bucket's hundreds of per-pass records collapse into one write.
 *  - READS are polled, not change-streamed: the serving app does one `find()`
 *    per `PollIntervalMs` instead of reacting to every write, and that `find()`
 *    is BOUNDED to the recently-changeable buckets (`UptimeSync.pollFilter`). So its cost
 *    scales with the number of services in that small window, not with the full
 *    24h of retained history — an unbounded poll re-read the whole collection
 *    every interval and, once every city scraped (so the collection tripled),
 *    dominated the serving vCPU. This replaced a per-write change stream
 *    (UPDATE_LOOKUP) whose full-document lookups pegged the serving vCPU at multi-city
 *    volume, and it drops the change stream's replica-set requirement.
 */
class UptimeMonitor(
  db: Option[MongoDatabase] = None,
  surfaceExternalWrites: Boolean = false,
  tagReloadIntervalMs: Long = UptimeMonitor.TagReloadIntervalMs
) extends Logging {
  import UptimeMonitor._

  /** Whether this process may REBUILD the bucket TTL index, or only create it when absent.
   *
   *  DERIVED, NOT PASSED. `surfaceExternalWrites` already says which tier this is — the
   *  serving app sets it, the worker leaves it off — and a second boolean at the call site is
   *  one a future wiring can silently forget, with nothing failing. Both tiers write
   *  `uptimeBuckets` and both authenticate as `kinowo_app`, so the serving app is perfectly
   *  able to drop an index the worker depends on. It has nothing to gain by doing so: both
   *  compute the expiry from the same constant, so a disagreement the reader can see is one
   *  the owner sees too, rebuilds, and reports if it cannot. */
  private val ownsIndexes: Boolean = !surfaceExternalWrites

  private val data = new BucketStore()
  private val listeners = new java.util.concurrent.CopyOnWriteArrayList[BucketListener]()

  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection("uptimeBuckets"))
  private val tagColl: Option[MongoCollection[Document]] = db.map(_.getCollection("uptimeServiceTags"))

  // The cross-process reads (boot hydrate + serving-app poll) share this
  // process's bucket map and listener fan-out; see `UptimeSync`. Package-private
  // so a spec can seed buckets the way a polled worker write would.
  private[services] val sync = new UptimeSync(data, notifyListeners)
  // Per-service labels ride their own collection; see `ServiceTags`.
  private val serviceTags = new ServiceTags(tagColl)

  // Daemon scheduler running the flusher (always) + poller (serving app only);
  // null until init runs, or if Mongo is absent.
  private val scheduler = new AtomicReference[ScheduledExecutorService]()

  // Index creation + Mongo hydrate run in a daemon thread so app start
  // doesn't block on Mongo round-trips. Records that arrive before
  // hydration finishes write to the in-memory bucket regardless; once
  // hydrate lands, counts merge additively.
  coll.foreach { c =>
    val thread = new Thread(() => {
      ensureIndexes(c)
      tagColl.foreach(serviceTags.ensureIndex)
      sync.hydrate(c)
      tagColl.foreach(serviceTags.load)
      // Schedule background work only AFTER hydrate: flushing absolute cumulative
      // state before the on-disk base is loaded would overwrite Mongo with just
      // this process's fresh increments.
      val exec = Executors.newScheduledThreadPool(2, (r: Runnable) => {
        val th = new Thread(r, "uptime-monitor"); th.setDaemon(true); th
      })
      scheduler.set(exec)
      backgroundSchedule(Some(c), tagColl).foreach { job =>
        exec.scheduleWithFixedDelay(job.task, job.periodMs, job.periodMs, TimeUnit.MILLISECONDS)
      }
      if (surfaceExternalWrites)
        logger.info(s"UptimeMonitor: polling uptimeBuckets every ${PollIntervalMs / 1000}s, reloading uptimeServiceTags every ${tagReloadIntervalMs / 1000}s for cross-process updates.")
    }, "uptime-monitor-init")
    thread.setDaemon(true)
    thread.start()
  }

  /** Create the compound index, and bring the bucket TTL index in line with
   *  `BucketTtlSeconds`. The TTL half is [[MongoTtlIndex.reconcile]], which reads
   *  the existing expiry back before touching anything — see its comment for why
   *  the `collMod` this used to fire unconditionally was both unauthorised and a
   *  no-op. The compound index keeps its own `Try` so a TTL that cannot be
   *  reconciled can't skip it.
   *
   *  `reconcile` only when this process OWNS the collection — see `ownsIndexes`. The
   *  serving app shares `uptimeBuckets` with the worker and gets `ensure`, which creates
   *  the index when absent and otherwise reports rather than rebuilding. */
  private def ensureIndexes(c: MongoCollection[Document]): Unit = {
    if (ownsIndexes) MongoTtlIndex.reconcile(c, "bucket", BucketTtlSeconds, "UptimeMonitor")
    else             MongoTtlIndex.ensure(c, "bucket", BucketTtlSeconds, "UptimeMonitor (read tier)")

    Try {
      Await.result(c.createIndex(
        Indexes.compoundIndex(Indexes.ascending("service"), Indexes.ascending("bucket"))
      ).toFuture(), 10.seconds)
    }.recover { case exception => logger.warn(s"Uptime compound index creation failed: ${exception.getMessage}") }
  }

  /** The recurring background work this monitor runs once hydrate lands, as data
   *  so the CADENCES are assertable without a scheduler or a Mongo round-trip.
   *  The writer side always flushes; the reader side (serving app) additionally
   *  polls the buckets and re-reads the tags — each on its OWN period, because
   *  the two reads cost wildly different amounts (see `TagReloadIntervalMs`). */
  private[services] def backgroundSchedule(
    bucketCollection: Option[MongoCollection[Document]],
    tagCollection: Option[MongoCollection[Document]]
  ): Seq[ScheduledJob] = {
    val flushJob = bucketCollection.map(c => ScheduledJob("flush", FlushIntervalMs, () => Try(flush(c))))
    val readerJobs =
      if (!surfaceExternalWrites) Seq.empty
      else Seq(
        bucketCollection.map(c => ScheduledJob("poll-buckets", PollIntervalMs, () => Try(sync.poll(c)))),
        // Re-read the tag collection so a cinema tagged by the worker after this
        // app booted still surfaces — but on its OWN, far slower period. The read
        // is unfiltered and the collection is no longer small (see
        // `TagReloadIntervalMs`), so it must not ride the 10s bucket poll.
        tagCollection.map(tc => ScheduledJob("reload-tags", tagReloadIntervalMs, () => Try(serviceTags.load(tc)))),
      ).flatten
    flushJob.toSeq ++ readerJobs
  }

  def addListener(f: BucketListener): Unit = { listeners.add(f); () }
  def removeListener(f: BucketListener): Unit = { listeners.remove(f); () }

  def recordSuccess(service: String): Unit = recordSuccess(service, None, fallback = false)

  /** Record a successful call along with how long it took, so the uptime page
   *  can show per-service average latency (1h + total). */
  def recordSuccess(service: String, durationMs: Long): Unit = recordSuccess(service, Some(durationMs), fallback = false)

  /** Record a success that was served via the Filmweb fallback — the primary
   *  scrape failed or came back empty, so the showtimes came from Filmweb. Counts
   *  as a success (the user got data) but also marks the bucket `fallback`, so the
   *  /uptime bar can show "served via Filmweb" rather than a plain green. */
  def recordFallbackSuccess(service: String, durationMs: Long): Unit =
    recordSuccess(service, Some(durationMs), fallback = true)

  private def recordSuccess(service: String, durationMs: Option[Long], fallback: Boolean): Unit = {
    val bucket = currentBucket(service)
    bucket.successes.incrementAndGet()
    durationMs.foreach { ms =>
      bucket.durationSumMs.addAndGet(ms)
      bucket.durationCount.incrementAndGet()
    }
    if (fallback) bucket.fallback.set(true)
    bucket.dirty.set(true)
    notifyListeners(service, bucket)
  }

  /** Record a call that completed without error but came back empty-handed —
   *  for a cinema scrape, the page loaded and parsed yet yielded zero
   *  screenings. That's neither a success (we got no data) nor a failure (the
   *  upstream didn't error), so it gets its own dimension and surfaces as a
   *  white "no screenings" bar. The round-trip was real, so its latency still
   *  counts toward the average. */
  def recordEmpty(service: String, durationMs: Long): Unit = {
    val bucket = currentBucket(service)
    bucket.zeroes.incrementAndGet()
    bucket.durationSumMs.addAndGet(durationMs)
    bucket.durationCount.incrementAndGet()
    bucket.dirty.set(true)
    notifyListeners(service, bucket)
  }

  /** Average duration (ms) of timed successful calls for `service` within the
   *  last hour, or None when nothing was timed in that window. */
  def averageMs1h(service: String): Option[Long] = averageMs(service, Some(60 * 60 * 1000L))

  /** Average duration (ms) of timed successful calls for `service` across all
   *  retained buckets (up to the 24h Mongo TTL), or None when none were timed. */
  def averageMsTotal(service: String): Option[Long] = averageMs(service, None)

  private def averageMs(service: String, windowMs: Option[Long]): Option[Long] = {
    val buckets = data.get(service)
    if (buckets == null) None
    else {
      val relevant = windowMs match {
        case Some(w) => buckets.tailMap(bucketTimestamp(System.currentTimeMillis() - w)).values()
        case None    => buckets.values()
      }
      var sum = 0L
      var cnt = 0L
      relevant.forEach { b => sum += b.durationSumMs.get(); cnt += b.durationCount.get() }
      if (cnt == 0L) None else Some(sum / cnt)
    }
  }

  def recordFailure(service: String, error: String): Unit = {
    val bucket = currentBucket(service)
    bucket.failures.incrementAndGet()
    if (bucket.errors.size() < MaxErrorsPerBucket) bucket.errors.add(error)
    bucket.dirty.set(true)
    notifyListeners(service, bucket)
  }

  def history(service: String): Seq[BucketSnapshot] = {
    val buckets = data.get(service)
    if (buckets == null) Seq.empty
    else buckets.values().asScala.toSeq.map(b =>
      BucketSnapshot(b.timestamp, b.successes.get(), b.failures.get(), b.zeroes.get(), b.errors.asScala.toSeq, b.fallback.get())
    )
  }

  /** The status keywords of the most recent `limit` buckets that recorded any
   *  activity, oldest→newest. This is everything a row's TRIAGE classification
   *  needs, and it deliberately does not build the row's bar series: the US
   *  registers one service per venue (5,031), so materialising all 96 slots for
   *  every service just to decide which few are red is ~484k objects — enough to
   *  OOM-kill the web pod, which is what it did on 2026-08-31. */
  def recentStatuses(service: String, limit: Int): Seq[String] = {
    val buckets = data.get(service)
    if (buckets == null) Seq.empty
    else {
      val out = List.newBuilder[String]
      var taken = 0
      val it = buckets.descendingMap().values().iterator()
      while (taken < limit && it.hasNext) {
        val b = it.next()
        val status = BucketSnapshot(b.timestamp, b.successes.get(), b.failures.get(), b.zeroes.get(), Seq.empty).status
        if (status != "empty") { out += status; taken += 1 }
      }
      out.result().reverse   // walked newest-first; callers want oldest→newest
    }
  }

  /** The error strings recorded by the most recent `limit` buckets that saw any
   *  activity — the same window [[recentStatuses]] classifies, so the two describe
   *  the same buckets. Cheap for the same reason: it walks the bucket map from the
   *  newest end and never materialises a row's 96 slots.
   *
   *  What it is FOR: telling a venue that is broken apart from one that no longer
   *  exists. Both are red, and only the error text separates them (see
   *  [[services.scrapes.GoneUpstream]]). */
  def recentErrors(service: String, limit: Int): Seq[String] = {
    val buckets = data.get(service)
    if (buckets == null) Seq.empty
    else {
      val out = List.newBuilder[String]
      var taken = 0
      val it = buckets.descendingMap().values().iterator()
      while (taken < limit && it.hasNext) {
        val b = it.next()
        val status = BucketSnapshot(b.timestamp, b.successes.get(), b.failures.get(), b.zeroes.get(), Seq.empty).status
        if (status != "empty") { out ++= b.errors.asScala; taken += 1 }
      }
      out.result()
    }
  }

  /** Every service's counts summed over the buckets at or after `cutoff` — what the
   *  Prometheus exposition needs, and nothing else.
   *
   *  It exists because the obvious spelling (`services.map(s => s -> history(s))`)
   *  is the /uptime OOM again, on a hot path: `history` materialises all 96 of a
   *  service's slots INCLUDING each one's error strings, and the US registers one
   *  service per venue (5,031), so a scrape allocated ~484k `BucketSnapshot`s plus
   *  their error lists — every 30 seconds, forever, in the 768m heap that also
   *  holds the read model. That is what OOM-killed `web-us` repeatedly, and since
   *  `pekko.jvm-exit-on-fatal-error` exits rather than limps, a scrape of an
   *  operational endpoint took the public site down for a restart.
   *
   *  The window is applied in the SKIP-LIST, not after the fact: `tailMap` touches
   *  only the two or three slots a 30-minute window spans, and one small
   *  [[RecentTotals]] per service replaces 96 snapshots. Services with no bucket in
   *  the window still appear, at zero — a gauge that vanishes when a service goes
   *  quiet reads as "no data" to an alert that needs to see the zero. */
  def recentTotals(cutoff: Long): Seq[(String, RecentTotals)] =
    data.entrySet().asScala.toSeq.map { entry =>
      var successes = 0
      var failures  = 0
      var zeroes    = 0
      val it = entry.getValue.tailMap(cutoff, true).values().iterator()
      while (it.hasNext) {
        val b = it.next()
        successes += b.successes.get()
        failures  += b.failures.get()
        zeroes    += b.zeroes.get()
      }
      entry.getKey -> RecentTotals(successes, failures, zeroes)
    }

  def services: Set[String] = data.keySet().asScala.toSet

  // ── Per-service tags (generic per-row labels) ────────────────────────────────

  /** Attach `tags` to `service`, replacing any existing set; returns whether a
   *  Mongo write was made. The rules — and why a no-op set must NOT write — are
   *  on [[ServiceTags.tagService]]. */
  def tagService(service: String, tags: Set[String]): Boolean = serviceTags.tagService(service, tags)

  /** Current per-service tags, for the page render. */
  def serviceTagsSnapshot(): Map[String, Set[String]] = serviceTags.snapshot()

  private def currentBucket(service: String): Bucket = {
    val timestamp = bucketTimestamp(System.currentTimeMillis())
    val buckets = bucketsOf(data, service)
    val bucket = bucketAt(buckets, timestamp)
    dropExpired(buckets, timestamp)
    bucket
  }

  // ── Batched writes ──────────────────────────────────────────────────────────

  /** Collect every bucket marked dirty since the last call, clearing the flag as
   *  we go, and snapshot its CUMULATIVE counts. `getAndSet(false)` BEFORE reading
   *  the counts means a record that lands mid-drain re-marks the bucket dirty, so
   *  it's caught next cycle rather than lost (worst case: written twice — `$set`
   *  is idempotent). Package-private so the flush cadence can be unit-tested
   *  without a Mongo round-trip. */
  private[services] def drainDirty(): Seq[BucketWrite] = {
    val out = Vector.newBuilder[BucketWrite]
    data.forEach { (service, buckets) =>
      buckets.values().forEach { b =>
        if (b.dirty.getAndSet(false))
          out += BucketWrite(service, b.timestamp,
            b.successes.get(), b.failures.get(), b.zeroes.get(),
            b.durationSumMs.get(), b.durationCount.get(),
            b.errors.asScala.toList, b.fallback.get())
      }
    }
    out.result()
  }

  private def flush(c: MongoCollection[Document]): Unit = drainDirty().foreach(writeBucket(c, _))

  /** Force a flush now (used on shutdown + in tests). No-op without Mongo. */
  private[services] def flushNow(): Unit = coll.foreach(flush)

  // Best-effort: a Mongo write failure must never break the flusher thread. The
  // `.subscribe(onError)` only catches async delivery errors — but the driver
  // builds the operation synchronously at `.subscribe`, so a closed client (Play
  // hot-reload, prod shutdown) throws `IllegalStateException: state should be:
  // open` right here, before any subscription exists. The Try is what keeps that
  // throw from killing the scheduled flush.
  private def writeBucket(c: MongoCollection[Document], bw: BucketWrite): Unit = Try {
    c.updateOne(
      Filters.and(Filters.eq("service", bw.service), Filters.eq("bucket", new java.util.Date(bw.bucketTimestamp))),
      Updates.combine(
        Updates.set("successes", bw.successes),
        Updates.set("failures", bw.failures),
        Updates.set("zeroes", bw.zeroes),
        Updates.set("durationSumMs", bw.durationSumMs),
        Updates.set("durationCount", bw.durationCount),
        Updates.set("errors", bw.errors.asJava),
        Updates.set("fallback", bw.fallback)
      ),
      new UpdateOptions().upsert(true)
    ).subscribe(
      (_: org.mongodb.scala.result.UpdateResult) => (),
      (exception: Throwable) => logger.debug(s"Uptime Mongo write failed: ${exception.getMessage}")
    )
  }.recover { case exception => logger.debug(s"Uptime Mongo write failed: ${exception.getMessage}") }.getOrElse(())

  private def notifyListeners(service: String, bucket: Bucket): Unit =
    if (!listeners.isEmpty) {
      val snap = BucketSnapshot(bucket.timestamp, bucket.successes.get(), bucket.failures.get(), bucket.zeroes.get(), bucket.errors.asScala.toSeq, bucket.fallback.get())
      listeners.forEach(f => Try(f(service, snap)))
    }

  /** Flush anything pending and stop the background scheduler. Idempotent; safe
   *  when nothing was started (no Mongo). */
  def close(): Unit = {
    flushNow()
    Option(scheduler.get()).foreach(e => Try(e.shutdown()))
  }
}

object UptimeMonitor {
  type BucketListener = (String, BucketSnapshot) => Unit

  val BucketDurationMs: Long = 15 * 60 * 1000L
  // Kept in lock-step with BucketDurationMs so the retained window stays 24h:
  // MaxBuckets * BucketDurationMs = 24h. The collection's TTL is one bucket
  // LONGER than this (see BucketTtlSeconds) so the oldest displayed slot survives.
  // It bounds both the in-memory cutoff and the /uptime timeline's slot count.
  val MaxBuckets: Int = 96
  // Persisted buckets live one slot (15 min) longer than the 24h display window
  // (MaxBuckets * BucketDurationMs). With a flat 24h TTL the OLDEST slot the
  // /uptime page renders can be deleted out from under it mid-window; the extra
  // bucket of margin keeps the full timeline populated. 24h15m = 87300s.
  val BucketTtlSeconds: Long = (MaxBuckets + 1).toLong * BucketDurationMs / 1000L
  val MaxErrorsPerBucket: Int = 10
  // Dirty buckets flush to Mongo this often (writer side).
  val FlushIntervalMs: Long = 10000L
  // The serving app re-reads the uptimeBuckets snapshot this often (reader side).
  val PollIntervalMs: Long = 10000L
  // The serving app re-reads uptimeServiceTags this often. Deliberately MUCH
  // slower than the bucket poll: `ServiceTags.load` is an UNFILTERED read of the whole
  // collection, which is no longer the handful of documents it was when it rode
  // the 10s poll — 2,687 tag documents for Poland alone (measured 2026-07-18),
  // i.e. ~8,640 reloads/day × 2,687 documents × 4 web machines, ~23M document
  // reads per day per instance for data that only changes when a cinema is
  // (re)tagged. Tags are static config, so a 5-minute cadence still surfaces a
  // newly tagged cinema well within the /uptime page's usefulness while cutting
  // that read volume 30×.
  val TagReloadIntervalMs: Long = 5 * 60 * 1000L
  // The poll only fetches buckets newer than this — older ones are frozen (writes
  // only ever hit the current slot) and were already loaded at boot. Must exceed
  // a slot + the final flush + slack for a poll delayed while the serving box is
  // under load: 3 slots (45 min) sits ~30 min past the freeze point. Bounds the
  // poll to the recent window instead of the full 24h history, so its cost stops
  // growing with retention (and with the scraper/cinema count behind it). Stays a
  // multiple of the slot, so the per-poll bucket count is flat regardless of size.
  val PollLookbackMs: Long = 3 * BucketDurationMs
  // Boot hydrate parameters. The fetch reads ~a full day of buckets in one go, so
  // give each attempt a generous budget (the old flat 10s timed out under deploy-
  // storm Mongo load, stranding the process with no history) and retry a few times
  // before giving up. Runs on a daemon thread — neither the timeout nor the
  // backoff blocks app start.
  val HydrateTimeout: FiniteDuration = 30.seconds
  val HydrateMaxAttempts: Int = 4
  val HydrateRetryBackoff: FiniteDuration = 2.seconds

  def bucketTimestamp(epochMs: Long): Long = epochMs - (epochMs % BucketDurationMs)

  /** service → (bucket timestamp → bucket): the in-memory state every cluster
   *  reads or writes. One instance per monitor, shared by reference with the
   *  [[UptimeSync]] that merges other processes' buckets into it. */
  type BucketStore = ConcurrentHashMap[String, ConcurrentSkipListMap[Long, Bucket]]

  private[services] def bucketsOf(store: BucketStore, service: String): ConcurrentSkipListMap[Long, Bucket] =
    store.computeIfAbsent(service, _ => new ConcurrentSkipListMap[Long, Bucket]())

  private[services] def bucketAt(buckets: ConcurrentSkipListMap[Long, Bucket], timestamp: Long): Bucket =
    buckets.computeIfAbsent(timestamp, t => Bucket(t))

  /** Forget the slots that fell out of the retained window as of `timestamp`. */
  private[services] def dropExpired(buckets: ConcurrentSkipListMap[Long, Bucket], timestamp: Long): Unit =
    buckets.headMap(timestamp - MaxBuckets * BucketDurationMs).clear()

  // The venue's public source-page URL travels as a `"url:<https…>"` service
  // tag (written by the worker's `CinemaClientMarkers`). Both /uptime and /debug
  // turn a cinema name into a link to it, so the parse lives here — one prefix,
  // one extractor — rather than re-spelt at each render site.
  val UrlTagPrefix: String = "url:"
  def urlFromTags(tags: Set[String]): Option[String] =
    tags.find(_.startsWith(UrlTagPrefix)).map(_.drop(UrlTagPrefix.length))

  /** `service name -> public source-page URL` for every tagged service carrying
   *  a `url:` tag, derived from a `serviceTagsSnapshot()`. The cinema pages look
   *  the URL up by `cinema.displayName`. */
  def cinemaUrls(tagsSnapshot: Map[String, Set[String]]): Map[String, String] =
    tagsSnapshot.flatMap { case (name, tags) => urlFromTags(tags).map(name -> _) }

  // Per-cinema detail-enrichment health is recorded under a synthetic
  // "<cinema displayName>|enrichment" service, distinct from the scrape service
  // (the bare cinema name). The `/uptime` page parses the suffix to render the
  // enrichment bar grouped under its cinema rather than as a standalone row.
  val EnrichmentSuffix: String = "|enrichment"
  def enrichmentService(cinemaDisplayName: String): String = cinemaDisplayName + EnrichmentSuffix
  def isEnrichmentService(service: String): Boolean = service.endsWith(EnrichmentSuffix)
  def baseCinemaOf(service: String): String = service.stripSuffix(EnrichmentSuffix)

  /** One recurring background job: what it is, how often it runs, and the work.
   *  Exists so the cadences are inspectable data rather than arguments buried in
   *  a `scheduleWithFixedDelay` call. */
  private[services] case class ScheduledJob(name: String, periodMs: Long, task: Runnable)

  case class Bucket(timestamp: Long) {
    val successes = new AtomicInteger(0)
    val failures  = new AtomicInteger(0)
    // Calls that returned without error but empty-handed (a scrape that parsed
    // cleanly yet found zero screenings). Tracked apart from successes/failures
    // so the bucket can surface as a white "no screenings" bar.
    val zeroes    = new AtomicInteger(0)
    val errors    = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    // Timing of *successful* calls: total ms and how many were timed. Kept
    // separate from `successes` so untimed successes (e.g. browser img events)
    // don't skew the average toward zero.
    val durationSumMs = new AtomicLong(0L)
    val durationCount = new AtomicInteger(0)
    // Set by record*; cleared by the flusher. Coalesces a bucket's many records
    // into one Mongo write per flush interval.
    val dirty = new AtomicBoolean(false)
    // Sticky within the slot: set when at least one call in this bucket was
    // served via the Filmweb fallback (primary down / empty). The slot still
    // counts as a success (the user got showtimes), so `status` stays green —
    // this just lets the /uptime page mark the bar "served via Filmweb".
    val fallback = new AtomicBoolean(false)
  }

  /** One service's counts over a time window — the whole of what the Prometheus
   *  exposition reads, deliberately without the error strings a [[BucketSnapshot]]
   *  carries. See [[UptimeMonitor.recentTotals]]. */
  case class RecentTotals(successes: Int, failures: Int, zeroes: Int)

  /** A bucket's cumulative counts captured for one flush. */
  case class BucketWrite(
    service: String, bucketTimestamp: Long,
    successes: Int, failures: Int, zeroes: Int,
    durationSumMs: Long, durationCount: Int,
    errors: List[String], fallback: Boolean = false
  )

  case class BucketSnapshot(timestamp: Long, successes: Int, failures: Int, zeroes: Int, errors: Seq[String], fallback: Boolean = false) {
    // Precedence: a failure dominates (red/yellow); a real success means green
    // even alongside a zero-result call in the same slot; only when every
    // non-failed call came back empty does the slot read "zero" (white). An
    // untouched slot is "empty" (no data).
    def status: String =
      if (successes + failures + zeroes == 0) "empty"
      else if (failures > 0) (if (successes > 0 || zeroes > 0) "yellow" else "red")
      else if (successes > 0) "green"
      else "zero"
  }
}
