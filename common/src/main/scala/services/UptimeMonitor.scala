package services

import com.mongodb.client.model.{IndexOptions => JIndexOptions, UpdateOptions}
import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument}
import java.util.concurrent.TimeUnit
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, Observer, ObservableFuture, SingleObservableFuture, Subscription, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Indexes, Updates}
import org.bson.conversions.Bson
import play.api.Logging

import java.util.concurrent.{ConcurrentHashMap, Executors, ScheduledExecutorService}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * @param watchExternalWrites when true, after hydrating from Mongo this instance
 *        opens a change stream on `uptimeBuckets` and merges every write —
 *        including those made by OTHER processes — into its in-memory map,
 *        firing listeners so the live /uptime SSE stream reflects them. The web
 *        (serving) process sets this so it surfaces the worker's scraper +
 *        enrichment metrics; the worker leaves it off — nothing reads its map.
 *
 * Persistence is BATCHED: `recordSuccess`/`recordFailure` only mutate the
 * in-memory bucket (cheap) and mark it dirty; a daemon flusher writes each dirty
 * bucket's CUMULATIVE counts to Mongo every `FlushIntervalMs` via `$set` (upsert).
 * The old per-call `$inc` write turned every scraper HTTP fetch into a Mongo
 * write AND a change-stream event — at multi-city scrape volume (hundreds of
 * fetches/pass) that pegged the serving app's change-stream consumer. Batching
 * collapses a pass's hundreds of writes per service+bucket into one flush, so the
 * change-stream volume the web app processes is bounded by the flush cadence, not
 * the fetch rate. `$set` of the cumulative is idempotent — the change stream
 * already applies post-images with SET semantics (see `applyExternalUpdate`).
 */
class UptimeMonitor(db: Option[MongoDatabase] = None, watchExternalWrites: Boolean = false) extends Logging {
  import UptimeMonitor._

  private val data = new ConcurrentHashMap[String, java.util.concurrent.ConcurrentSkipListMap[Long, Bucket]]()
  private val listeners = new java.util.concurrent.CopyOnWriteArrayList[BucketListener]()

  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection("uptimeBuckets"))

  // Index creation + Mongo hydrate run in a daemon thread so app start
  // doesn't block on Mongo round-trips. Records that arrive before
  // hydration finishes write to the in-memory bucket regardless; once
  // hydrate lands, counts merge additively.
  coll.foreach { c =>
    val t = new Thread(() => {
      Try {
        Await.result(c.createIndex(
          Indexes.ascending("bucket"),
          new JIndexOptions().expireAfter(24L, TimeUnit.HOURS)
        ).toFuture(), 10.seconds)
        Await.result(c.createIndex(
          Indexes.compoundIndex(Indexes.ascending("service"), Indexes.ascending("bucket"))
        ).toFuture(), 10.seconds)
      }.recover { case ex => logger.warn(s"Uptime index creation failed: ${ex.getMessage}") }
      hydrate(c)
      // Open the change stream only after hydrate finishes, so the one-time
      // additive hydrate can't race the replacing change-stream applies on the
      // same bucket. Gated to the serving process — the worker reads nothing.
      if (watchExternalWrites) startChangeStream(c)
      // Start the flusher AFTER hydrate too: flushing absolute cumulative state
      // before the on-disk base is loaded would overwrite Mongo with just this
      // process's fresh increments.
      startFlusher(c)
    }, "uptime-monitor-hydrate")
    t.setDaemon(true)
    t.start()
  }

  // Subscription for the external-write change stream (web process only), so
  // shutdown can cancel it. Null until startChangeStream runs.
  private val watchSub = new AtomicReference[Subscription]()
  // Daemon flusher; null until startFlusher runs (or if Mongo is absent).
  private val flusher = new AtomicReference[ScheduledExecutorService]()

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
      BucketSnapshot(b.timestamp, b.successes.get(), b.failures.get(), b.errors.asScala.toSeq)
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

  // ── Batched persistence ─────────────────────────────────────────────────────

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
            b.successes.get(), b.failures.get(),
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

  private def startFlusher(c: MongoCollection[Document]): Unit = {
    val exec = Executors.newSingleThreadScheduledExecutor { r =>
      val th = new Thread(r, "uptime-monitor-flush"); th.setDaemon(true); th
    }
    exec.scheduleWithFixedDelay(() => Try(flush(c)), FlushIntervalMs, FlushIntervalMs, TimeUnit.MILLISECONDS)
    flusher.set(exec)
  }

  private def notifyListeners(service: String, bucket: Bucket): Unit =
    if (!listeners.isEmpty) {
      val snap = BucketSnapshot(bucket.timestamp, bucket.successes.get(), bucket.failures.get(), bucket.errors.asScala.toSeq)
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

  /** Apply a bucket post-image that originated in another process (the worker),
   *  delivered by the Mongo change stream. The full document carries the
   *  CUMULATIVE totals for that service+bucket, so we SET rather than add —
   *  applying the same event twice (driver resume, or the web app's own echoed
   *  write) is idempotent. Then notify listeners so the live /uptime SSE stream
   *  reflects worker activity this process never recorded itself. Package-private:
   *  the only callers are the change stream and its test. */
  private[services] def applyExternalUpdate(
    service: String, bucketTs: Long,
    successes: Int, failures: Int,
    durationSumMs: Long, durationCount: Int,
    errors: Seq[String]
  ): Unit = {
    val ts = bucketTimestamp(bucketTs)
    val buckets = data.computeIfAbsent(service, _ => new java.util.concurrent.ConcurrentSkipListMap[Long, Bucket]())
    val bucket = buckets.computeIfAbsent(ts, t => Bucket(t))
    bucket.successes.set(successes)
    bucket.failures.set(failures)
    bucket.durationSumMs.set(durationSumMs)
    bucket.durationCount.set(durationCount)
    bucket.errors.clear()
    errors.take(MaxErrorsPerBucket).foreach(bucket.errors.add)
    // A stream-applied bucket is NOT dirty — only locally-recorded ones flush, so
    // the web app never echoes the worker's writes back to Mongo.
    val cutoff = ts - MaxBuckets * BucketDurationMs
    buckets.headMap(cutoff).clear()
    notifyListeners(service, bucket)
  }

  /** Open a change stream on `uptimeBuckets` and merge every write — including
   *  the worker's — into the in-memory map via `applyExternalUpdate`.
   *  `UPDATE_LOOKUP` makes update events carry the full post-image, so each
   *  event has the bucket's cumulative counts. The driver auto-resumes across
   *  transient blips; a terminal error just logs (the page then shows data only
   *  as fresh as the next web restart's hydrate). Requires a replica set; on a
   *  standalone Mongo the stream errors out and we degrade to that snapshot. */
  private def startChangeStream(c: MongoCollection[Document]): Unit = Try {
    c.watch().fullDocument(FullDocument.UPDATE_LOOKUP)
      .subscribe(new Observer[ChangeStreamDocument[Document]] {
        override def onSubscribe(s: Subscription): Unit = { watchSub.set(s); s.request(Long.MaxValue) }
        override def onNext(change: ChangeStreamDocument[Document]): Unit =
          Option(change.getFullDocument).foreach { doc =>
            try
              for {
                service    <- Option(doc.getString("service"))
                bucketDate <- Option(doc.getDate("bucket"))
              } applyExternalUpdate(
                service, bucketDate.getTime,
                doc.getInteger("successes", 0),
                doc.getInteger("failures", 0),
                Try(doc.get("durationSumMs").map(_.asNumber().longValue()).getOrElse(0L)).getOrElse(0L),
                doc.getInteger("durationCount", 0),
                Try(doc.getList("errors", classOf[String])).toOption.fold(Seq.empty[String])(_.asScala.toSeq)
              )
            catch { case ex: Throwable => logger.warn(s"Uptime change-stream apply failed: ${ex.getMessage}") }
          }
        override def onError(e: Throwable): Unit =
          logger.warn(s"Uptime change stream ended (${e.getMessage}) — /uptime now shows only the last hydrate snapshot.")
        override def onComplete(): Unit = ()
      })
    logger.info("UptimeMonitor: watching uptimeBuckets change stream for cross-process updates.")
  }.recover { case ex => logger.warn(s"Uptime change-stream watch failed to start: ${ex.getMessage}") }

  /** Flush anything pending and stop the background flusher + change-stream
   *  subscription. Idempotent; safe when neither was started (no Mongo). */
  def close(): Unit = {
    flushNow()
    Option(flusher.get()).foreach(e => Try(e.shutdown()))
    Option(watchSub.get()).foreach(s => Try(s.unsubscribe()))
  }
}

object UptimeMonitor {
  type BucketListener = (String, BucketSnapshot) => Unit

  val BucketDurationMs: Long = 5 * 60 * 1000L
  val MaxBuckets: Int = 288
  val MaxErrorsPerBucket: Int = 10
  // How often dirty buckets are flushed to Mongo. The serving app's change-stream
  // load is ~(distinct services touched per interval) / interval, so this caps it
  // well below the raw scraper fetch rate while keeping /uptime within ~10s live.
  val FlushIntervalMs: Long = 10000L

  def bucketTimestamp(epochMs: Long): Long = epochMs - (epochMs % BucketDurationMs)

  case class Bucket(timestamp: Long) {
    val successes = new AtomicInteger(0)
    val failures  = new AtomicInteger(0)
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
    successes: Int, failures: Int,
    durationSumMs: Long, durationCount: Int,
    errors: List[String]
  )

  case class BucketSnapshot(timestamp: Long, successes: Int, failures: Int, errors: Seq[String]) {
    def status: String =
      if (successes + failures == 0) "empty"
      else if (failures == 0) "green"
      else if (successes == 0) "red"
      else "yellow"
  }
}
