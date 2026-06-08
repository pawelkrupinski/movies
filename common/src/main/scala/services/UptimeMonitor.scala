package services

import com.mongodb.client.model.{IndexOptions => JIndexOptions, UpdateOptions}
import java.util.concurrent.TimeUnit
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Indexes, Updates}
import org.mongodb.scala.bson.conversions.Bson
import play.api.Logging

import java.util.concurrent.{ConcurrentHashMap, Executors, ScheduledExecutorService}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * @param surfaceExternalWrites when true, after hydrating from Mongo this
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
 *    is BOUNDED to the recently-changeable buckets (`pollFilter`). So its cost
 *    scales with the number of services in that small window, not with the full
 *    24h of retained history — an unbounded poll re-read the whole collection
 *    every interval and, once every city scraped (so the collection tripled),
 *    dominated the serving vCPU. This replaced a per-write change stream
 *    (UPDATE_LOOKUP) whose full-doc lookups pegged the serving vCPU at multi-city
 *    volume, and it drops the change stream's replica-set requirement.
 */
class UptimeMonitor(db: Option[MongoDatabase] = None, surfaceExternalWrites: Boolean = false) extends Logging {
  import UptimeMonitor._

  private val data = new ConcurrentHashMap[String, java.util.concurrent.ConcurrentSkipListMap[Long, Bucket]]()
  private val listeners = new java.util.concurrent.CopyOnWriteArrayList[BucketListener]()

  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection("uptimeBuckets"))

  // Daemon scheduler running the flusher (always) + poller (serving app only);
  // null until init runs, or if Mongo is absent.
  private val scheduler = new AtomicReference[ScheduledExecutorService]()

  // Index creation + Mongo hydrate run in a daemon thread so app start
  // doesn't block on Mongo round-trips. Records that arrive before
  // hydration finishes write to the in-memory bucket regardless; once
  // hydrate lands, counts merge additively.
  coll.foreach { c =>
    val t = new Thread(() => {
      ensureIndexes(c)
      hydrate(c)
      // Schedule background work only AFTER hydrate: flushing absolute cumulative
      // state before the on-disk base is loaded would overwrite Mongo with just
      // this process's fresh increments.
      val exec = Executors.newScheduledThreadPool(2, (r: Runnable) => {
        val th = new Thread(r, "uptime-monitor"); th.setDaemon(true); th
      })
      scheduler.set(exec)
      exec.scheduleWithFixedDelay(() => Try(flush(c)), FlushIntervalMs, FlushIntervalMs, TimeUnit.MILLISECONDS)
      if (surfaceExternalWrites) {
        exec.scheduleWithFixedDelay(() => Try(poll(c)), PollIntervalMs, PollIntervalMs, TimeUnit.MILLISECONDS)
        logger.info(s"UptimeMonitor: polling uptimeBuckets every ${PollIntervalMs / 1000}s for cross-process updates.")
      }
    }, "uptime-monitor-init")
    t.setDaemon(true)
    t.start()
  }

  /** Create the TTL + compound indexes, and bring an EXISTING bucket-TTL index's
   *  expiry in line with `BucketTtlSeconds`. Each step is isolated: a TTL change
   *  makes `createIndex` throw `IndexOptionsConflict` (it can create but never
   *  ALTER), so that step is expected to fail on an already-indexed collection —
   *  the `collMod` is what actually applies the new expiry there. Keeping the
   *  compound index in its own `Try` means the TTL conflict can't skip it. */
  private def ensureIndexes(c: MongoCollection[Document]): Unit = {
    Try {
      Await.result(c.createIndex(
        Indexes.ascending("bucket"),
        new JIndexOptions().expireAfter(BucketTtlSeconds, TimeUnit.SECONDS)
      ).toFuture(), 10.seconds)
    }.recover { case ex => logger.debug(s"Uptime TTL index not (re)created — collMod will reconcile: ${ex.getMessage}") }

    db.foreach { database =>
      Try {
        val collMod = new org.bson.Document("collMod", c.namespace.getCollectionName)
          .append("index", new org.bson.Document("keyPattern", new org.bson.Document("bucket", 1))
            .append("expireAfterSeconds", BucketTtlSeconds))
        Await.result(database.runCommand(collMod).toFuture(), 10.seconds)
      }.recover { case ex => logger.debug(s"Uptime TTL collMod skipped: ${ex.getMessage}") }
    }

    Try {
      Await.result(c.createIndex(
        Indexes.compoundIndex(Indexes.ascending("service"), Indexes.ascending("bucket"))
      ).toFuture(), 10.seconds)
    }.recover { case ex => logger.warn(s"Uptime compound index creation failed: ${ex.getMessage}") }
  }

  def addListener(f: BucketListener): Unit = { listeners.add(f); () }
  def removeListener(f: BucketListener): Unit = { listeners.remove(f); () }

  def recordSuccess(service: String): Unit = recordSuccess(service, None)

  /** Record a successful call along with how long it took, so the uptime page
   *  can show per-service average latency (1h + total). */
  def recordSuccess(service: String, durationMs: Long): Unit = recordSuccess(service, Some(durationMs))

  private def recordSuccess(service: String, durationMs: Option[Long]): Unit = {
    val bucket = currentBucket(service)
    bucket.successes.incrementAndGet()
    durationMs.foreach { ms =>
      bucket.durationSumMs.addAndGet(ms)
      bucket.durationCount.incrementAndGet()
    }
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
      BucketSnapshot(b.timestamp, b.successes.get(), b.failures.get(), b.zeroes.get(), b.errors.asScala.toSeq)
    )
  }

  def services: Set[String] = data.keySet().asScala.toSet

  private def currentBucket(service: String): Bucket = {
    val ts = bucketTimestamp(System.currentTimeMillis())
    val buckets = data.computeIfAbsent(service, _ => new java.util.concurrent.ConcurrentSkipListMap[Long, Bucket]())
    val bucket = buckets.computeIfAbsent(ts, t => Bucket(t))
    val cutoff = ts - MaxBuckets * BucketDurationMs
    buckets.headMap(cutoff).clear()
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
            b.errors.asScala.toList)
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
      Filters.and(Filters.eq("service", bw.service), Filters.eq("bucket", new java.util.Date(bw.bucketTs))),
      Updates.combine(
        Updates.set("successes", bw.successes),
        Updates.set("failures", bw.failures),
        Updates.set("zeroes", bw.zeroes),
        Updates.set("durationSumMs", bw.durationSumMs),
        Updates.set("durationCount", bw.durationCount),
        Updates.set("errors", bw.errors.asJava)
      ),
      new UpdateOptions().upsert(true)
    ).subscribe(
      (_: org.mongodb.scala.result.UpdateResult) => (),
      (ex: Throwable) => logger.debug(s"Uptime Mongo write failed: ${ex.getMessage}")
    )
  }.recover { case ex => logger.debug(s"Uptime Mongo write failed: ${ex.getMessage}") }.getOrElse(())

  // ── Polled reads (serving app) ──────────────────────────────────────────────

  /** The poll only needs buckets that can still change. Writes only ever land in
   *  the CURRENT 15-min slot (see `currentBucket`), so a bucket is frozen once its
   *  slot closes and its final cumulative count flushes — within
   *  `BucketDurationMs + FlushIntervalMs` of the slot start. Everything older was
   *  already loaded by the boot `hydrate` and never changes again, so re-reading
   *  it every interval is pure waste (it dominated the serving box's CPU once the
   *  scraper count — hence the collection — grew). Bound the poll to a generous
   *  recent window; `PollLookbackMs` carries the margin rationale. The
   *  `{bucket: {$gte}}` range is served by the existing `{bucket:1}` TTL index. */
  private[services] def pollFilter(nowMs: Long): Bson =
    Filters.gte("bucket", new java.util.Date(nowMs - PollLookbackMs))

  /** Read the recently-changeable `uptimeBuckets` and merge them in. One bounded
   *  query per interval — cost scales with the number of services in the window,
   *  not with the full 24h of retained history. */
  private def poll(c: MongoCollection[Document]): Unit = Try {
    val docs = Await.result(c.find(pollFilter(System.currentTimeMillis())).toFuture(), 10.seconds)
    docs.foreach { doc =>
      for {
        service    <- Option(doc.getString("service"))
        bucketDate <- Option(doc.getDate("bucket"))
      } {
        val ts = bucketTimestamp(bucketDate.getTime)
        // Don't clobber a bucket this process has un-flushed local changes for
        // (its own recorded services, e.g. web's OAuth fetches) — the next flush
        // will reconcile it. External (worker) buckets are never locally dirty.
        val locallyDirty = Option(data.get(service)).flatMap(b => Option(b.get(ts))).exists(_.dirty.get())
        if (!locallyDirty)
          applyExternalUpdate(
            service, bucketDate.getTime,
            doc.getInteger("successes", 0),
            doc.getInteger("failures", 0),
            doc.getInteger("zeroes", 0),
            Try(doc.get("durationSumMs").map(_.asNumber().longValue()).getOrElse(0L)).getOrElse(0L),
            doc.getInteger("durationCount", 0),
            Try(doc.getList("errors", classOf[String])).toOption.fold(Seq.empty[String])(_.asScala.toSeq)
          )
      }
    }
  }.recover { case ex => logger.warn(s"Uptime poll failed: ${ex.getMessage}") }

  private def notifyListeners(service: String, bucket: Bucket): Unit =
    if (!listeners.isEmpty) {
      val snap = BucketSnapshot(bucket.timestamp, bucket.successes.get(), bucket.failures.get(), bucket.zeroes.get(), bucket.errors.asScala.toSeq)
      listeners.forEach(f => Try(f(service, snap)))
    }

  private def hydrate(c: MongoCollection[Document]): Unit = Try {
    val docs = Await.result(c.find().toFuture(), 10.seconds)
    var count = 0
    docs.foreach { doc =>
      for {
        service <- Option(doc.getString("service"))
        bucketDate <- Option(doc.getDate("bucket"))
      } {
        val ts = bucketTimestamp(bucketDate.getTime)
        val buckets = data.computeIfAbsent(service, _ => new java.util.concurrent.ConcurrentSkipListMap[Long, Bucket]())
        val bucket = buckets.computeIfAbsent(ts, t => Bucket(t))
        bucket.successes.addAndGet(doc.getInteger("successes", 0))
        bucket.failures.addAndGet(doc.getInteger("failures", 0))
        bucket.zeroes.addAndGet(doc.getInteger("zeroes", 0))
        bucket.durationSumMs.addAndGet(Try(doc.get("durationSumMs").map(_.asNumber().longValue()).getOrElse(0L)).getOrElse(0L))
        bucket.durationCount.addAndGet(doc.getInteger("durationCount", 0))
        Try(doc.getList("errors", classOf[String])).toOption.foreach { errs =>
          errs.asScala.take(MaxErrorsPerBucket).foreach(bucket.errors.add)
        }
        count += 1
      }
    }
    if (count > 0) logger.info(s"Hydrated $count uptime bucket(s) from Mongo.")
  }.recover { case ex => logger.warn(s"Uptime hydrate failed: ${ex.getMessage}") }

  /** Merge a bucket post-image that originated in another process (the worker),
   *  read by the poller. The snapshot carries the CUMULATIVE totals for that
   *  service+bucket, so we SET rather than add — re-applying the same snapshot
   *  (every poll re-reads it) is idempotent. Listeners fire only when something
   *  actually changed, so an unchanged poll doesn't spam the /uptime SSE.
   *  Package-private: the only callers are the poller and its test. */
  private[services] def applyExternalUpdate(
    service: String, bucketTs: Long,
    successes: Int, failures: Int, zeroes: Int,
    durationSumMs: Long, durationCount: Int,
    errors: Seq[String]
  ): Unit = {
    val ts = bucketTimestamp(bucketTs)
    val buckets = data.computeIfAbsent(service, _ => new java.util.concurrent.ConcurrentSkipListMap[Long, Bucket]())
    val bucket = buckets.computeIfAbsent(ts, t => Bucket(t))
    val cappedErrors = errors.take(MaxErrorsPerBucket)
    val changed =
      bucket.successes.get() != successes ||
      bucket.failures.get() != failures ||
      bucket.zeroes.get() != zeroes ||
      bucket.durationSumMs.get() != durationSumMs ||
      bucket.durationCount.get() != durationCount ||
      bucket.errors.asScala.toSeq != cappedErrors
    bucket.successes.set(successes)
    bucket.failures.set(failures)
    bucket.zeroes.set(zeroes)
    bucket.durationSumMs.set(durationSumMs)
    bucket.durationCount.set(durationCount)
    bucket.errors.clear()
    cappedErrors.foreach(bucket.errors.add)
    val cutoff = ts - MaxBuckets * BucketDurationMs
    buckets.headMap(cutoff).clear()
    if (changed) notifyListeners(service, bucket)
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
  // The poll only fetches buckets newer than this — older ones are frozen (writes
  // only ever hit the current slot) and were already loaded at boot. Must exceed
  // a slot + the final flush + slack for a poll delayed while the serving box is
  // under load: 3 slots (45 min) sits ~30 min past the freeze point. Bounds the
  // poll to the recent window instead of the full 24h history, so its cost stops
  // growing with retention (and with the scraper/cinema count behind it). Stays a
  // multiple of the slot, so the per-poll bucket count is flat regardless of size.
  val PollLookbackMs: Long = 3 * BucketDurationMs

  def bucketTimestamp(epochMs: Long): Long = epochMs - (epochMs % BucketDurationMs)

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
  }

  /** A bucket's cumulative counts captured for one flush. */
  case class BucketWrite(
    service: String, bucketTs: Long,
    successes: Int, failures: Int, zeroes: Int,
    durationSumMs: Long, durationCount: Int,
    errors: List[String]
  )

  case class BucketSnapshot(timestamp: Long, successes: Int, failures: Int, zeroes: Int, errors: Seq[String]) {
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
