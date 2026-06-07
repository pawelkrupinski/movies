package services.tasks

import com.mongodb.MongoWriteException
import com.mongodb.client.model.{IndexOptions => JIndexOptions}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.model.{Filters, FindOneAndUpdateOptions, Indexes, ReturnDocument, Updates}

import play.api.Logging

import java.time.Instant
import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Mongo-backed `TaskQueue`, collection `tasks`. Document shape:
 * {{{
 *   { _id: <uuid>, taskType: "ScrapeCinema", dedupKey: "scrape|kino-x",
 *     payload: { ... },              // string→string sub-doc
 *     state: "waiting"|"worked_on"|"deleted",
 *     active: Bool,                  // true iff state != deleted (drives the dedup index)
 *     submittedAt: ISODate, attempts: Int,
 *     workerId: String?, leaseExpiresAt: ISODate?, deletedAt: ISODate? }
 * }}}
 *
 * Idempotent enqueue and atomic claim are expressed declaratively through
 * indexes and `findOneAndUpdate`, so there's no business logic to keep in sync
 * with [[InMemoryTaskQueue]] — the redundancy/skip decision lives in handlers,
 * above this boundary.
 *
 * Dedup uses a **unique partial index** on `dedupKey` over `active: true`.
 * Partial filters only allow equality/$exists (not `$in`), hence the explicit
 * `active` flag rather than a `state ∈ {waiting,worked_on}` predicate. A
 * `deleted` tombstone (active:false) is outside the index, so re-enqueuing a
 * finished key is allowed; concurrent inserts race on the index and the loser's
 * duplicate-key error is caught as `Duplicate` (the `MongoLock` idiom).
 *
 * Blocking `.toFuture()` throughout — callers are daemon worker/reaper threads.
 */
class MongoTaskQueue(db: Option[MongoDatabase] = None) extends TaskQueue with Logging {

  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection("tasks"))

  coll.foreach { c =>
    val t = new Thread(() => createIndexes(c), "tasks-init")
    t.setDaemon(true)
    t.start()
  }

  private def createIndexes(c: MongoCollection[Document]): Unit = Try {
    Await.result(c.createIndex(
      Indexes.ascending("dedupKey"),
      new JIndexOptions().unique(true).partialFilterExpression(Filters.eq("active", true))
    ).toFuture(), 10.seconds)
    Await.result(c.createIndex(Indexes.compoundIndex(Indexes.ascending("state"), Indexes.ascending("submittedAt"))).toFuture(), 10.seconds)
    Await.result(c.createIndex(Indexes.compoundIndex(Indexes.ascending("state"), Indexes.ascending("leaseExpiresAt"))).toFuture(), 10.seconds)
    Await.result(c.createIndex(
      Indexes.ascending("deletedAt"),
      new JIndexOptions().expireAfter(6L, TimeUnit.HOURS).partialFilterExpression(Filters.exists("deletedAt", true))
    ).toFuture(), 10.seconds)
  }.recover { case ex => logger.warn(s"Task queue index creation failed: ${ex.getMessage}") }

  override def enqueue(
    taskType:    TaskType,
    dedupKey:    String,
    payload:     Map[String, String],
    submittedAt: Instant
  ): EnqueueResult = coll match {
    case None => EnqueueResult.Duplicate
    case Some(c) =>
      val filter = Filters.and(Filters.eq("dedupKey", dedupKey), Filters.eq("active", true))
      val onInsert = Updates.combine(
        Updates.setOnInsert("_id", UUID.randomUUID().toString),
        Updates.setOnInsert("taskType", taskType.name),
        Updates.setOnInsert("dedupKey", dedupKey),
        Updates.setOnInsert("payload", payloadDoc(payload)),
        Updates.setOnInsert("state", TaskState.Waiting),
        Updates.setOnInsert("active", true),
        Updates.setOnInsert("submittedAt", new java.util.Date(submittedAt.toEpochMilli)),
        Updates.setOnInsert("attempts", 0)
      )
      Try {
        val res = Await.result(c.updateOne(filter, onInsert, new com.mongodb.client.model.UpdateOptions().upsert(true)).toFuture(), 10.seconds)
        if (res.getUpsertedId != null) EnqueueResult.Added else EnqueueResult.Duplicate
      }.recover {
        case ex: MongoWriteException if Option(ex.getError).exists(_.getCode == 11000) => EnqueueResult.Duplicate
        case ex: Throwable =>
          logger.warn(s"TaskQueue.enqueue($dedupKey) failed: ${ex.getMessage}")
          EnqueueResult.Duplicate
      }.getOrElse(EnqueueResult.Duplicate)
  }

  override def claim(workerId: String, lease: FiniteDuration, now: Instant): Option[Task] = coll match {
    case None => None
    case Some(c) =>
      val update = Updates.combine(
        Updates.set("state", TaskState.WorkedOn),
        Updates.set("workerId", workerId),
        Updates.set("leaseExpiresAt", new java.util.Date(now.plusMillis(lease.toMillis).toEpochMilli)),
        Updates.inc("attempts", 1)
      )
      val opts = FindOneAndUpdateOptions()
        .sort(Indexes.ascending("submittedAt"))
        .returnDocument(ReturnDocument.AFTER)
      Try {
        Await.result(c.findOneAndUpdate(Filters.eq("state", TaskState.Waiting), update, opts).toFutureOption(), 10.seconds)
          .map(toTask)
      }.recover {
        case ex: Throwable =>
          logger.warn(s"TaskQueue.claim failed: ${ex.getMessage}")
          None
      }.getOrElse(None)
  }

  override def complete(id: String, workerId: String): Unit = coll.foreach { c =>
    val filter = Filters.and(Filters.eq("_id", id), Filters.eq("state", TaskState.WorkedOn), Filters.eq("workerId", workerId))
    val update = Updates.combine(
      Updates.set("state", TaskState.Deleted),
      Updates.set("active", false),
      Updates.set("deletedAt", new java.util.Date(Instant.now().toEpochMilli)),
      Updates.unset("workerId"),
      Updates.unset("leaseExpiresAt")
    )
    Try(Await.result(c.updateOne(filter, update).toFuture(), 10.seconds))
      .recover { case ex => logger.warn(s"TaskQueue.complete($id) failed: ${ex.getMessage}") }
  }

  override def release(id: String, workerId: String, error: Option[String]): Unit = coll.foreach { c =>
    val filter = Filters.and(Filters.eq("_id", id), Filters.eq("state", TaskState.WorkedOn), Filters.eq("workerId", workerId))
    val base = Updates.combine(
      Updates.set("state", TaskState.Waiting),
      Updates.unset("workerId"),
      Updates.unset("leaseExpiresAt")
    )
    val update = error.fold(base)(e => Updates.combine(base, Updates.set("lastError", e)))
    Try(Await.result(c.updateOne(filter, update).toFuture(), 10.seconds))
      .recover { case ex => logger.warn(s"TaskQueue.release($id) failed: ${ex.getMessage}") }
  }

  override def reapExpiredLeases(now: Instant): Int = coll match {
    case None => 0
    case Some(c) =>
      val filter = Filters.and(
        Filters.eq("state", TaskState.WorkedOn),
        Filters.lt("leaseExpiresAt", new java.util.Date(now.toEpochMilli))
      )
      val update = Updates.combine(
        Updates.set("state", TaskState.Waiting),
        Updates.unset("workerId"),
        Updates.unset("leaseExpiresAt")
      )
      Try {
        Await.result(c.updateMany(filter, update).toFuture(), 10.seconds).getModifiedCount.toInt
      }.recover {
        case ex: Throwable =>
          logger.warn(s"TaskQueue.reapExpiredLeases failed: ${ex.getMessage}")
          0
      }.getOrElse(0)
  }

  override def countByState(): Map[String, Long] = coll match {
    case None => Map.empty
    case Some(c) =>
      Seq(TaskState.Waiting, TaskState.WorkedOn, TaskState.Deleted).flatMap { state =>
        Try(Await.result(c.countDocuments(Filters.eq("state", state)).toFuture(), 10.seconds)).toOption
          .filter(_ > 0).map(state -> _)
      }.toMap
  }

  private def payloadDoc(payload: Map[String, String]): org.mongodb.scala.bson.BsonDocument = {
    val bd = new org.mongodb.scala.bson.BsonDocument()
    payload.foreach { case (k, v) => bd.put(k, BsonString(v)) }
    bd
  }

  private def toTask(doc: Document): Task = {
    val payload = doc.get("payload")
      .filter(_.isDocument)
      .map { v =>
        val d = v.asDocument()
        d.keySet().asScala.iterator.map(k => k -> d.getString(k).getValue).toMap
      }
      .getOrElse(Map.empty[String, String])
    Task(
      id       = doc.getString("_id"),
      taskType = TaskType.byName(doc.getString("taskType")).getOrElse(
        throw new IllegalStateException(s"Unknown taskType ${doc.getString("taskType")}")),
      dedupKey = doc.getString("dedupKey"),
      payload  = payload,
      attempts = doc.getInteger("attempts", 0)
    )
  }
}
