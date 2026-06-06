package services

import com.mongodb.client.model.{IndexOptions => JIndexOptions, UpdateOptions}
import java.util.concurrent.TimeUnit
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Indexes, Updates}
import org.bson.conversions.Bson
import play.api.Logging

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

class UptimeMonitor(db: Option[MongoDatabase] = None) extends Logging {
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
    }, "uptime-monitor-hydrate")
    t.setDaemon(true)
    t.start()
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
    mongoUpsertSuccess(service, durationMs)
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
    mongoUpsertFailure(service, error)
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

  private def mongoUpsertSuccess(service: String, durationMs: Option[Long]): Unit = {
    val timing = durationMs.toList.flatMap(ms =>
      List(Updates.inc("durationSumMs", ms), Updates.inc("durationCount", 1)))
    upsertBucket(service, Updates.combine((
      Updates.inc("successes", 1) ::
      timing :::
      List(Updates.setOnInsert("failures", 0),
           Updates.setOnInsert("errors", java.util.Collections.emptyList[String]())))*))
  }

  private def mongoUpsertFailure(service: String, error: String): Unit =
    upsertBucket(service, Updates.combine(
      Updates.inc("failures", 1),
      Updates.push("errors", error),
      Updates.setOnInsert("successes", 0)
    ))

  // Uptime recording is best-effort: a Mongo write failure must never break
  // the caller (a scraper recording its own attempt). The `.subscribe(onError)`
  // only catches async delivery errors — but the driver builds the operation
  // synchronously at `.subscribe`, so a closed client (Play hot-reload, prod
  // shutdown) throws `IllegalStateException: state should be: open` right here,
  // before any subscription exists. The Try is what actually keeps that throw
  // from escaping into the scrape's failure path.
  private def upsertBucket(service: String, update: Bson): Unit = coll.foreach { c =>
    Try {
      val ts = bucketTimestamp(System.currentTimeMillis())
      c.updateOne(
        Filters.and(Filters.eq("service", service), Filters.eq("bucket", new java.util.Date(ts))),
        update,
        new UpdateOptions().upsert(true)
      ).subscribe(
        (_: org.mongodb.scala.result.UpdateResult) => (),
        (ex: Throwable) => logger.debug(s"Uptime Mongo write failed: ${ex.getMessage}")
      )
    }.recover { case ex => logger.debug(s"Uptime Mongo write failed: ${ex.getMessage}") }
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
}

object UptimeMonitor {
  type BucketListener = (String, BucketSnapshot) => Unit

  val BucketDurationMs: Long = 5 * 60 * 1000L
  val MaxBuckets: Int = 288
  val MaxErrorsPerBucket: Int = 10

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
  }

  case class BucketSnapshot(timestamp: Long, successes: Int, failures: Int, errors: Seq[String]) {
    def status: String =
      if (successes + failures == 0) "empty"
      else if (failures == 0) "green"
      else if (successes == 0) "red"
      else "yellow"
  }
}
