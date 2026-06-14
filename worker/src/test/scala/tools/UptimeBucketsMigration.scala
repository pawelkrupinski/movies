package tools

import org.mongodb.scala.{ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Updates}
import com.mongodb.client.model.UpdateOptions
import services.{MongoConnection, UptimeMonitor}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 *  One-shot: re-bucket the `uptimeBuckets` collection from the old 5-minute
 *  granularity to the current `UptimeMonitor.BucketDurationMs` (15 min),
 *  MERGING the sub-buckets that now fall in the same 15-min slot — summing
 *  their counts and concatenating their errors (capped at
 *  `MaxErrorsPerBucket`) — into a single document keyed at the 15-min
 *  boundary, then deleting the redundant sub-boundary docs.
 *
 *  Why a migration rather than relying on the app's hydrate (which already
 *  sums old docs into 15-min buckets in memory): the stale sub-boundary docs
 *  otherwise linger for the bucket TTL, where the serving app's poll re-applies
 *  them with SET semantics (last-write-wins clobber) and a reboot's hydrate
 *  double-counts any slot whose boundary doc was already flushed-merged. This
 *  collapses Mongo to exactly one doc per (service, 15-min slot).
 *
 *  Only FROZEN slots are touched: it skips the current and previous slot (the
 *  ones writers may still be mutating / may carry a flushed-merged boundary
 *  doc from a deploy that landed inside them), so it never double-counts an
 *  active slot. Idempotent — a slot already collapsed to one boundary doc
 *  merges to itself.
 *
 *  Reads `MONGODB_URI` / `MONGODB_DB` from env — source `.env.local` first:
 *
 *  ```
 *  set -a; . ./.env.local; set +a
 *  sbt 'worker/Test/runMain tools.UptimeBucketsMigration'
 *  ```
 */
object UptimeBucketsMigration {

  /** One raw uptime document, as read from Mongo. */
  case class RawBucket(
    service: String, bucketTs: Long,
    successes: Int, failures: Int,
    durationSumMs: Long, durationCount: Int,
    errors: Seq[String])

  /** One merged document, keyed at the 15-min boundary. */
  case class MergedBucket(
    service: String, bucketTs: Long,
    successes: Int, failures: Int,
    durationSumMs: Long, durationCount: Int,
    errors: Seq[String])

  /** Pure core: group rows by (service, 15-min boundary), summing the counts
   *  and concatenating errors in time order (capped). Boundary math + cap come
   *  from `UptimeMonitor` so this can't drift from how the app re-buckets on
   *  hydrate. */
  def merge15Min(rows: Seq[RawBucket]): Seq[MergedBucket] =
    rows
      .groupBy(r => (r.service, UptimeMonitor.bucketTimestamp(r.bucketTs)))
      .map { case ((service, ts), group) =>
        val ordered = group.sortBy(_.bucketTs)
        MergedBucket(
          service, ts,
          ordered.map(_.successes).sum,
          ordered.map(_.failures).sum,
          ordered.map(_.durationSumMs).sum,
          ordered.map(_.durationCount).sum,
          ordered.flatMap(_.errors).take(UptimeMonitor.MaxErrorsPerBucket))
      }
      .toSeq

  def main(args: Array[String]): Unit = {
    val connection = MongoConnection.fromEnv(required = false)
    try {
      val db = connection.database.getOrElse {
        println("MONGODB_URI not set — nothing to do.")
        sys.exit(1)
      }
      val coll = db.getCollection("uptimeBuckets")

      // Skip the current + previous slot: writers may still be flushing them,
      // and a deploy that landed inside one leaves a flushed-merged boundary
      // doc that summing would double-count. Everything older is frozen.
      val cutoff = UptimeMonitor.bucketTimestamp(System.currentTimeMillis()) - UptimeMonitor.BucketDurationMs

      val docs = Await.result(coll.find().toFuture(), 60.seconds)
      val rows = docs.flatMap { doc =>
        for {
          service    <- Option(doc.getString("service"))
          bucketDate <- Option(doc.getDate("bucket"))
        } yield RawBucket(
          service, bucketDate.getTime,
          doc.getInteger("successes", 0),
          doc.getInteger("failures", 0),
          Try(doc.get("durationSumMs").map(_.asNumber().longValue()).getOrElse(0L)).getOrElse(0L),
          doc.getInteger("durationCount", 0),
          Try(doc.getList("errors", classOf[String])).toOption.fold(Seq.empty[String])(_.asScala.toSeq))
      }.toSeq

      val frozen = rows.filter(r => UptimeMonitor.bucketTimestamp(r.bucketTs) < cutoff)
      val merged = merge15Min(frozen)
      println(s"Read ${rows.size} doc(s); ${frozen.size} in frozen slots collapse to ${merged.size} 15-min bucket(s).")

      var collapsed = 0
      merged.foreach { m =>
        val boundary = new java.util.Date(m.bucketTs)
        // Persist the merged total at the boundary FIRST (upsert), so the data
        // survives before any delete; then drop the redundant sub-boundary docs
        // in this 15-min window. Crash-safe: a re-run reconciles.
        Await.result(coll.updateOne(
          Filters.and(Filters.eq("service", m.service), Filters.eq("bucket", boundary)),
          Updates.combine(
            Updates.set("successes", m.successes),
            Updates.set("failures", m.failures),
            Updates.set("durationSumMs", m.durationSumMs),
            Updates.set("durationCount", m.durationCount),
            Updates.set("errors", m.errors.asJava)),
          new UpdateOptions().upsert(true)
        ).toFuture(), 30.seconds)

        val del = Await.result(coll.deleteMany(Filters.and(
          Filters.eq("service", m.service),
          Filters.gte("bucket", boundary),
          Filters.lt("bucket", new java.util.Date(m.bucketTs + UptimeMonitor.BucketDurationMs)),
          Filters.ne("bucket", boundary)
        )).toFuture(), 30.seconds)
        collapsed += del.getDeletedCount.toInt
      }
      println(s"Done: wrote ${merged.size} boundary doc(s), deleted $collapsed redundant sub-bucket doc(s).")
    } finally connection.close()
  }
}
