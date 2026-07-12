package services.tasks

import com.mongodb.WriteConcern
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, UpdateOptions, Updates}
import play.api.Logging

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Mongo-backed [[BulkTaskResultStore]], collection `bulk_task_results`. Exactly
 * one document per bulk `taskType` (keyed on `_id = taskType.name`), overwritten
 * on each run:
 * {{{
 *   { _id: "RefreshAllImdb", ranAt: ISODate(...), succeeded: true,
 *     message: "tick done in 812ms — 4 changed, 0 failed, 574 unchanged.",
 *     walked: 578, changed: 4, discovered: 0, failed: 0 }
 * }}}
 *
 * The worker writes here from [[BulkRefreshHandler]] when a corpus walk finishes;
 * the web reads it for the `/tasks` page. It's a tiny, bounded collection (≤ one
 * row per bulk job type), so `latest()` is an unbounded `find()` by design.
 *
 * Relaxed `{w:1, j:false}` write concern like [[MongoTaskQueue]]: a lost ack just
 * means the page shows a slightly stale last-run line until the next run — this is
 * operator diagnostics, not a system of record. Blocking `.toFuture()` throughout
 * (callers are daemon worker threads / a request thread).
 */
class MongoBulkTaskResultStore(db: Option[MongoDatabase] = None, collectionName: String = "bulk_task_results")
  extends BulkTaskResultStore with Logging {

  private val coll: Option[MongoCollection[Document]] =
    db.map(_.getCollection(collectionName).withWriteConcern(WriteConcern.W1.withJournal(false)))

  override def record(result: BulkTaskResult): Unit = coll.foreach { c =>
    val set = Seq(
      Some(Updates.set("ranAt", new java.util.Date(result.ranAt.toEpochMilli))),
      Some(Updates.set("succeeded", result.succeeded)),
      Some(Updates.set("message", result.message)),
      result.walked.map(Updates.set("walked", _)),
      result.changed.map(Updates.set("changed", _)),
      result.discovered.map(Updates.set("discovered", _)),
      result.failed.map(Updates.set("failed", _))
    ).flatten
    // Clear stale counts a message-only run leaves behind (e.g. a job that used to
    // report counts now runs generic), so the page never shows a phantom tally.
    val unset = Seq(
      result.walked.isEmpty     -> "walked",
      result.changed.isEmpty    -> "changed",
      result.discovered.isEmpty -> "discovered",
      result.failed.isEmpty     -> "failed"
    ).collect { case (true, field) => Updates.unset(field) }
    val update = Updates.combine((set ++ unset)*)
    Try(Await.result(
      c.updateOne(Filters.eq("_id", result.taskType.name), update, new UpdateOptions().upsert(true)).toFuture(),
      10.seconds))
      .recover { case exception => logger.warn(s"BulkTaskResultStore.record(${result.taskType.name}) failed: ${exception.getMessage}") }
  }

  override def latest(): Map[TaskType, BulkTaskResult] = coll match {
    case None => Map.empty
    case Some(c) =>
      Try(Await.result(c.find().toFuture(), 10.seconds).flatMap(toResult).map(r => r.taskType -> r).toMap)
        .recover { case exception =>
          logger.warn(s"BulkTaskResultStore.latest failed: ${exception.getMessage}")
          Map.empty[TaskType, BulkTaskResult]
        }.getOrElse(Map.empty)
  }

  private def toResult(document: Document): Option[BulkTaskResult] =
    TaskType.byName(document.getString("_id")).map { taskType =>
      def intOpt(field: String): Option[Int] =
        document.get(field).filter(_.isNumber).map(_.asNumber().intValue())
      BulkTaskResult(
        taskType   = taskType,
        ranAt      = Instant.ofEpochMilli(document.getDate("ranAt").getTime),
        succeeded  = document.get("succeeded").filter(_.isBoolean).exists(_.asBoolean().getValue),
        message    = Option(document.getString("message")).getOrElse(""),
        walked     = intOpt("walked"),
        changed    = intOpt("changed"),
        discovered = intOpt("discovered"),
        failed     = intOpt("failed")
      )
    }
}
