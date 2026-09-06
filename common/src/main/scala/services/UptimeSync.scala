package services

import org.mongodb.scala.{Document, MongoCollection, ObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.bson.conversions.Bson
import play.api.Logging
import tools.RetryWithBackoff

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/** The cross-process half of [[UptimeMonitor]]: what a process READS from
 *  `uptimeBuckets` — the boot `hydrate` of the display window and, on the
 *  serving app, the bounded `poll` that merges buckets OTHER processes (the
 *  worker) wrote. Both write into the monitor's own bucket store, which is why
 *  it is handed in by reference rather than owned here: `recordSuccess` and a
 *  polled worker bucket must land in the same map the query API reads.
 *
 *  `notify` is the monitor's listener fan-out, so a merged external bucket
 *  reaches the /uptime SSE exactly like a local record does. */
private[services] final class UptimeSync(
  buckets: UptimeMonitor.BucketStore,
  notify: (String, UptimeMonitor.Bucket) => Unit
) extends Logging {
  import UptimeMonitor._
  import UptimeSync._

  /** Read the recently-changeable `uptimeBuckets` and merge them in. One bounded
   *  query per interval — cost scales with the number of services in the window,
   *  not with the full 24h of retained history. */
  def poll(c: MongoCollection[Document]): Unit = Try {
    val documents = Await.result(c.find(pollFilter(System.currentTimeMillis())).toFuture(), 10.seconds)
    documents.foreach { document =>
      for {
        service    <- Option(document.getString("service"))
        bucketDate <- Option(document.getDate("bucket"))
      } {
        val timestamp = bucketTimestamp(bucketDate.getTime)
        // Don't clobber a bucket this process has un-flushed local changes for
        // (its own recorded services, e.g. web's OAuth fetches) — the next flush
        // will reconcile it. External (worker) buckets are never locally dirty.
        val locallyDirty = Option(buckets.get(service)).flatMap(b => Option(b.get(timestamp))).exists(_.dirty.get())
        if (!locallyDirty)
          applyExternalUpdate(
            service, bucketDate.getTime,
            document.getInteger("successes", 0),
            document.getInteger("failures", 0),
            document.getInteger("zeroes", 0),
            Try(document.get("durationSumMs").map(_.asNumber().longValue()).getOrElse(0L)).getOrElse(0L),
            document.getInteger("durationCount", 0),
            Try(document.getList("errors", classOf[String])).toOption.fold(Seq.empty[String])(_.asScala.toSeq),
            Try(document.getBoolean("fallback", false)).getOrElse(false)
          )
      }
    }
  }.recover { case exception => logger.warn(s"Uptime poll failed: ${exception.getMessage}") }

  /** Load the retained display window into the in-memory map at boot. The fetch
   *  is wrapped in `RetryWithBackoff`: a single 10s timeout used to STRAND the
   *  process with no history (the /uptime page then showed only the ~poll window,
   *  not the full 24h) whenever Mongo was briefly slow at boot — e.g. during a
   *  deploy storm. A transient slowdown must not be permanent, so retry with a
   *  generous per-attempt timeout before giving up. Runs on a daemon thread, so
   *  neither the timeout nor the backoff blocks app start, and records arriving
   *  mid-hydrate merge additively. Only the FETCH retries — the merge runs once on
   *  the materialised documents, so a retry can't double-count via `addAndGet`. */
  def hydrate(c: MongoCollection[Document]): Unit = Try {
    val documents = RetryWithBackoff("Uptime hydrate", maxAttempts = HydrateMaxAttempts, initialBackoff = HydrateRetryBackoff) {
      Await.result(c.find(hydrateFilter(System.currentTimeMillis())).toFuture(), HydrateTimeout)
    }
    var count = 0
    documents.foreach { document =>
      for {
        service <- Option(document.getString("service"))
        bucketDate <- Option(document.getDate("bucket"))
      } {
        val bucket = bucketAt(bucketsOf(buckets, service), bucketTimestamp(bucketDate.getTime))
        bucket.successes.addAndGet(document.getInteger("successes", 0))
        bucket.failures.addAndGet(document.getInteger("failures", 0))
        bucket.zeroes.addAndGet(document.getInteger("zeroes", 0))
        bucket.durationSumMs.addAndGet(Try(document.get("durationSumMs").map(_.asNumber().longValue()).getOrElse(0L)).getOrElse(0L))
        bucket.durationCount.addAndGet(document.getInteger("durationCount", 0))
        Try(document.getList("errors", classOf[String])).toOption.foreach { errs =>
          errs.asScala.take(MaxErrorsPerBucket).foreach(bucket.errors.add)
        }
        if (Try(document.getBoolean("fallback", false)).getOrElse(false)) bucket.fallback.set(true)
        count += 1
      }
    }
    if (count > 0) logger.info(s"Hydrated $count uptime bucket(s) from Mongo.")
  }.recover { case exception => logger.warn(s"Uptime hydrate failed after $HydrateMaxAttempts attempts: ${exception.getMessage}") }

  /** Merge a bucket post-image that originated in another process (the worker),
   *  read by the poller. The snapshot carries the CUMULATIVE totals for that
   *  service+bucket, so we SET rather than add — re-applying the same snapshot
   *  (every poll re-reads it) is idempotent. Listeners fire only when something
   *  actually changed, so an unchanged poll doesn't spam the /uptime SSE. */
  def applyExternalUpdate(
    service: String, rawTimestamp: Long,
    successes: Int, failures: Int, zeroes: Int,
    durationSumMs: Long, durationCount: Int,
    errors: Seq[String], fallback: Boolean = false
  ): Unit = {
    val timestamp = bucketTimestamp(rawTimestamp)
    val serviceBuckets = bucketsOf(buckets, service)
    val bucket = bucketAt(serviceBuckets, timestamp)
    val cappedErrors = errors.take(MaxErrorsPerBucket)
    val changed =
      bucket.successes.get() != successes ||
      bucket.failures.get() != failures ||
      bucket.zeroes.get() != zeroes ||
      bucket.durationSumMs.get() != durationSumMs ||
      bucket.durationCount.get() != durationCount ||
      bucket.errors.asScala.toSeq != cappedErrors ||
      bucket.fallback.get() != fallback
    bucket.successes.set(successes)
    bucket.failures.set(failures)
    bucket.zeroes.set(zeroes)
    bucket.durationSumMs.set(durationSumMs)
    bucket.durationCount.set(durationCount)
    bucket.fallback.set(fallback)
    bucket.errors.clear()
    cappedErrors.foreach(bucket.errors.add)
    dropExpired(serviceBuckets, timestamp)
    if (changed) notify(service, bucket)
  }
}

private[services] object UptimeSync {
  import UptimeMonitor._

  /** The poll only needs buckets that can still change. Writes only ever land in
   *  the CURRENT 15-min slot (see `UptimeMonitor.currentBucket`), so a bucket is
   *  frozen once its slot closes and its final cumulative count flushes — within
   *  `BucketDurationMs + FlushIntervalMs` of the slot start. Everything older was
   *  already loaded by the boot `hydrate` and never changes again, so re-reading
   *  it every interval is pure waste (it dominated the serving box's CPU once the
   *  scraper count — hence the collection — grew). Bound the poll to a generous
   *  recent window; `PollLookbackMs` carries the margin rationale. The
   *  `{bucket: {$gte}}` range is served by the existing `{bucket:1}` TTL index. */
  def pollFilter(nowMs: Long): Bson =
    Filters.gte("bucket", new java.util.Date(nowMs - PollLookbackMs))

  /** The hydrate only needs the buckets the /uptime page actually renders — the
   *  most recent `MaxBuckets` slots. Bounding to that window (a) skips older documents
   *  that linger inside the TTL margin but never display, and (b) rides the
   *  `{bucket:1}` index instead of a full collection scan. */
  def hydrateFilter(nowMs: Long): Bson =
    Filters.gte("bucket", new java.util.Date(bucketTimestamp(nowMs) - MaxBuckets.toLong * BucketDurationMs))
}
