package services

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters._

class UptimeMonitor {
  import UptimeMonitor._

  private val data = new ConcurrentHashMap[String, java.util.concurrent.ConcurrentSkipListMap[Long, Bucket]]()

  def recordSuccess(service: String): Unit = currentBucket(service).successes.incrementAndGet()
  def recordFailure(service: String): Unit = currentBucket(service).failures.incrementAndGet()

  def history(service: String): Seq[BucketSnapshot] = {
    val buckets = data.get(service)
    if (buckets == null) Seq.empty
    else buckets.values().asScala.toSeq.map(b =>
      BucketSnapshot(b.timestamp, b.successes.get(), b.failures.get())
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
}

object UptimeMonitor {
  val BucketDurationMs: Long = 5 * 60 * 1000L
  val MaxBuckets: Int = 288

  def bucketTimestamp(epochMs: Long): Long = epochMs - (epochMs % BucketDurationMs)

  case class Bucket(timestamp: Long) {
    val successes = new AtomicInteger(0)
    val failures  = new AtomicInteger(0)
  }

  case class BucketSnapshot(timestamp: Long, successes: Int, failures: Int) {
    def status: String =
      if (successes + failures == 0) "empty"
      else if (failures == 0) "green"
      else if (successes == 0) "red"
      else "yellow"
  }
}
