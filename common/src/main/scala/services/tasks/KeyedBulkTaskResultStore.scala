package services.tasks

import org.mongodb.scala.bson.{BsonBoolean, BsonDateTime, BsonInt32, BsonString}
import org.mongodb.scala.{Document, MongoDatabase, documentToUntypedDocument}
import play.api.Logging
import services.keyed.{DocCodec, KeyedDocStore, MongoKeyedDocStore}

import java.time.Instant
import scala.util.Try

/**
 * The [[BulkTaskResultStore]] over its rows — exactly one per bulk `taskType` (keyed on
 * `_id = taskType.name`), overwritten on each run; Mongo and in-memory differ only in the rows.
 * Operator diagnostics, not a system of record: a failed write or read is logged, and the page
 * shows a slightly stale (or no) last-run line until the next run.
 */
class KeyedBulkTaskResultStore(rows: KeyedDocStore[BulkTaskResult]) extends BulkTaskResultStore with Logging {
  override def record(result: BulkTaskResult): Unit =
    Try(rows.put(result.taskType.name, result))
      .recover { case exception => logger.warn(s"BulkTaskResultStore.record(${result.taskType.name}) failed: ${exception.getMessage}") }

  override def latest(): Map[TaskType, BulkTaskResult] =
    Try(rows.all().values.map(r => r.taskType -> r).toMap)
      .recover { case exception =>
        logger.warn(s"BulkTaskResultStore.latest failed: ${exception.getMessage}")
        Map.empty[TaskType, BulkTaskResult]
      }.getOrElse(Map.empty)
}

object KeyedBulkTaskResultStore {
  /**
   * {{{
   *   { _id: "RefreshAllImdb", ranAt: ISODate(...), succeeded: true,
   *     message: "tick done in 812ms — 4 changed, 0 failed, 574 unchanged.",
   *     walked: 578, changed: 4, discovered: 0, failed: 0 }
   * }}}
   * A message-only run writes no counts, so the page never shows a phantom tally left by an
   * earlier counted one.
   */
  val Codec: DocCodec[BulkTaskResult] = new DocCodec[BulkTaskResult] {
    def encode(result: BulkTaskResult): Document = {
      val counts = Seq("walked" -> result.walked, "changed" -> result.changed, "discovered" -> result.discovered, "failed" -> result.failed)
        .collect { case (field, Some(count)) => field -> BsonInt32(count) }
      // By name: the order the store's earlier `$set` upsert wrote them in, so every row reads alike.
      Document((counts ++ Seq("ranAt" -> BsonDateTime(result.ranAt.toEpochMilli), "succeeded" -> BsonBoolean(result.succeeded),
        "message" -> BsonString(result.message))).sortBy(_._1))
    }

    def decode(document: Document): Option[BulkTaskResult] =
      TaskType.byName(document.getString("_id")).flatMap { taskType =>
        def intOpt(field: String): Option[Int] =
          document.get(field).filter(_.isNumber).map(_.asNumber().intValue())
        document.get("ranAt").filter(_.isDateTime).map { ranAt =>
          BulkTaskResult(
            taskType   = taskType,
            ranAt      = Instant.ofEpochMilli(ranAt.asDateTime.getValue),
            succeeded  = document.get("succeeded").filter(_.isBoolean).exists(_.asBoolean().getValue),
            message    = Option(document.getString("message")).getOrElse(""),
            walked     = intOpt("walked"),
            changed    = intOpt("changed"),
            discovered = intOpt("discovered"),
            failed     = intOpt("failed")
          )
        }
      }
  }
}

/**
 * Mongo-backed [[BulkTaskResultStore]], collection `bulk_task_results`, with the task queue's
 * relaxed `{w:1, j:false}` write concern ([[MongoTaskQueue.QueueWriteConcern]]). The worker writes
 * here from [[BulkRefreshHandler]] when a corpus walk finishes; the web reads it for the `/tasks`
 * page. A tiny, bounded collection (≤ one row per bulk job type).
 */
class MongoBulkTaskResultStore(db: Option[MongoDatabase]) extends KeyedBulkTaskResultStore(
  db.fold(KeyedDocStore.absent[BulkTaskResult])(d => new MongoKeyedDocStore(
    d.getCollection(MongoBulkTaskResultStore.Collection).withWriteConcern(MongoTaskQueue.QueueWriteConcern), KeyedBulkTaskResultStore.Codec)))

object MongoBulkTaskResultStore {
  val Collection = "bulk_task_results"
}
