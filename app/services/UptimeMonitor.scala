package services

import com.mongodb.client.model.{IndexOptions => JIndexOptions, UpdateOptions}
import java.util.concurrent.TimeUnit
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Indexes, Updates}
import play.api.Logging

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

class UptimeMonitor(db: Option[MongoDatabase] = None) extends Logging {
  import UptimeMonitor._

  private val data = new ConcurrentHashMap[String, java.util.concurrent.ConcurrentSkipListMap[Long, Bucket]]()
  private val listeners = new java.util.concurrent.CopyOnWriteArrayList[BucketListener]()

  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection("uptimeBuckets"))

  coll.foreach { c =>
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
  }

  def addListener(f: BucketListener): Unit = { listeners.add(f); () }
  def removeListener(f: BucketListener): Unit = { listeners.remove(f); () }

  def recordSuccess(service: String): Unit = {
    val bucket = currentBucket(service)
    bucket.successes.incrementAndGet()
    mongoUpsertSuccess(service)
    notifyListeners(service, bucket)
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

  private def mongoUpsertSuccess(service: String): Unit = coll.foreach { c =>
    val ts = bucketTimestamp(System.currentTimeMillis())
    c.updateOne(
      Filters.and(Filters.eq("service", service), Filters.eq("bucket", new java.util.Date(ts))),
      Updates.combine(
        Updates.inc("successes", 1),
        Updates.setOnInsert("failures", 0),
        Updates.setOnInsert("errors", java.util.Collections.emptyList[String]())
      ),
      new UpdateOptions().upsert(true)
    ).subscribe(
      (_: org.mongodb.scala.result.UpdateResult) => (),
      (ex: Throwable) => logger.debug(s"Uptime Mongo write failed: ${ex.getMessage}")
    )
  }

  private def mongoUpsertFailure(service: String, error: String): Unit = coll.foreach { c =>
    val ts = bucketTimestamp(System.currentTimeMillis())
    c.updateOne(
      Filters.and(Filters.eq("service", service), Filters.eq("bucket", new java.util.Date(ts))),
      Updates.combine(
        Updates.inc("failures", 1),
        Updates.push("errors", error),
        Updates.setOnInsert("successes", 0)
      ),
      new UpdateOptions().upsert(true)
    ).subscribe(
      (_: org.mongodb.scala.result.UpdateResult) => (),
      (ex: Throwable) => logger.debug(s"Uptime Mongo write failed: ${ex.getMessage}")
    )
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
  }

  case class BucketSnapshot(timestamp: Long, successes: Int, failures: Int, errors: Seq[String]) {
    def status: String =
      if (successes + failures == 0) "empty"
      else if (failures == 0) "green"
      else if (successes == 0) "red"
      else "yellow"
  }
}
