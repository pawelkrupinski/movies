package services.tasks

import com.mongodb.{MongoWriteException, WriteConcern}
import com.mongodb.client.model.{IndexOptions => JIndexOptions}
import com.mongodb.client.model.changestream.{ChangeStreamDocument, OperationType}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, Observer, ObservableFuture, SingleObservableFuture, Subscription, documentToUntypedDocument}
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.model.{Aggregates, Filters, FindOneAndUpdateOptions, Indexes, ReturnDocument, Updates}

import play.api.Logging

import java.time.Instant
import java.util.UUID
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Mongo-backed `TaskQueue`, collection `tasks`. Document shape:
 * {{{
 *   { _id: <uuid>, taskType: "ScrapeCinema", dedupKey: "scrape|kino-x",
 *     payload: { ... },              // string→string sub-document
 *     state: "waiting"|"worked_on",  // "deleted" only survives transiently, pre-deploy — see `complete`
 *     active: Bool,                  // always true now (drives the dedup index)
 *     submittedAt: ISODate, attempts: Int,
 *     workerId: String?, leaseExpiresAt: ISODate? }
 * }}}
 *
 * Idempotent enqueue and atomic claim are expressed declaratively through
 * indexes and `findOneAndUpdate`, so there's no business logic to keep in sync
 * with [[InMemoryTaskQueue]] — the redundancy/skip decision lives in handlers,
 * above this boundary.
 *
 * Dedup uses a **unique partial index** on `dedupKey` over `active: true`.
 * Partial filters only allow equality/$exists (not `$in`), hence the explicit
 * `active` flag rather than a `state ∈ {waiting,worked_on}` predicate. `complete`
 * deletes the document, so a finished key has no row in the index and re-enqueuing
 * it is allowed; concurrent inserts race on the index and the loser's duplicate-key
 * error is caught as `Duplicate` (see [[services.MongoErrors]]).
 *
 * Blocking `.toFuture()` throughout — callers are daemon worker/reaper threads.
 *
 * Writes use a relaxed `{w:1, j:false}` write concern ([[MongoTaskQueue.QueueWriteConcern]]):
 * the queue is ephemeral bookkeeping, not a system of record, so paying the
 * journal-fsync + majority wait on every claim/complete/release/reap/enqueue is
 * wasted cost — on the shared-cpu Mongo it was the dominant per-op latency (the
 * `waitForWriteConcernDurationMillis` in the slow-op log). An ack lost to a crash
 * inside the ~100ms journal window self-heals: a lost `claim` leaves the task
 * waiting, a lost `complete` leaves a `worked_on` task whose lease the reaper
 * returns, and every handler is freshness-gated + idempotent so the re-run is a
 * `Skipped`. Atomic, multi-machine-safe claiming is unaffected — it comes from
 * `findOneAndUpdate` on the primary, not from the write concern.
 */
class MongoTaskQueue(db: Option[MongoDatabase] = None, collectionName: String = "tasks") extends TaskQueue with Logging {
  import MongoTaskQueue.QueueWriteConcern

  private val coll: Option[MongoCollection[Document]] =
    db.map(_.getCollection(collectionName).withWriteConcern(QueueWriteConcern))

  /** The effective write concern of the `tasks` collection — for diagnostics/tests. */
  def collectionWriteConcern: Option[WriteConcern] = coll.map(_.writeConcern)

  coll.foreach { c =>
    val thread = new Thread(() => createIndexes(c), "tasks-init")
    thread.setDaemon(true)
    thread.start()
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
  }.recover { case exception => logger.warn(s"Task queue index creation failed: ${exception.getMessage}") }

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
        Updates.setOnInsert("payload", payloadDocument(payload)),
        Updates.setOnInsert("state", TaskState.Waiting),
        Updates.setOnInsert("active", true),
        Updates.setOnInsert("submittedAt", new java.util.Date(submittedAt.toEpochMilli)),
        Updates.setOnInsert("attempts", 0)
      )
      Try {
        val res = Await.result(c.updateOne(filter, onInsert, new com.mongodb.client.model.UpdateOptions().upsert(true)).toFuture(), 10.seconds)
        if (res.getUpsertedId != null) EnqueueResult.Added else EnqueueResult.Duplicate
      }.recover {
        case exception: MongoWriteException if services.MongoErrors.isDuplicateKey(exception) => EnqueueResult.Duplicate
        case exception: Throwable =>
          logger.warn(s"TaskQueue.enqueue($dedupKey) failed: ${exception.getMessage}")
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
      // Eligible = waiting AND not held back by a backoff window still in the
      // future (a task never released with `notBefore` has no `nextEligibleAt`).
      val eligible = Filters.and(
        Filters.eq("state", TaskState.Waiting),
        Filters.or(
          Filters.exists("nextEligibleAt", false),
          Filters.lte("nextEligibleAt", new java.util.Date(now.toEpochMilli))))
      Try {
        Await.result(c.findOneAndUpdate(eligible, update, opts).headOption(), 10.seconds)
          .map(toTask)
      }.recover {
        case exception: Throwable =>
          logger.warn(s"TaskQueue.claim failed: ${exception.getMessage}")
          None
      }.getOrElse(None)
  }

  // Finishing a task DELETES the document outright rather than tombstoning it
  // (state=deleted + a 6h TTL). A tombstone bought nothing the delete doesn't: the
  // dedup index is partial on `active:true`, so a gone doc allows re-enqueue exactly
  // as a `active:false` tombstone did; `monitor` never listed deleted rows anyway;
  // and the finished-count lives in the Prometheus counter, not the collection. The
  // payoff is a `tasks` collection holding only the few active tasks instead of ~6h
  // of tombstones (≈thousands) — smaller working set + faster claim index scans —
  // and one change-stream/oplog event per completion (the delete) instead of two
  // (the update, then the TTL delete). The ownership guard is unchanged, so a late
  // call from a reaped worker still can't remove a task another worker reclaimed.
  // (The `deleted`-state + TTL index remain only to age out tombstones written by
  // the pre-deploy build; they can be dropped once those have expired.)
  override def complete(id: String, workerId: String): Unit = coll.foreach { c =>
    val filter = Filters.and(Filters.eq("_id", id), Filters.eq("state", TaskState.WorkedOn), Filters.eq("workerId", workerId))
    Try(Await.result(c.deleteOne(filter).toFuture(), 10.seconds))
      .recover { case exception => logger.warn(s"TaskQueue.complete($id) failed: ${exception.getMessage}") }
  }

  override def release(id: String, workerId: String, error: Option[String],
                       notBefore: Option[Instant]): Unit = coll.foreach { c =>
    val filter = Filters.and(Filters.eq("_id", id), Filters.eq("state", TaskState.WorkedOn), Filters.eq("workerId", workerId))
    // Set the backoff window, or clear a stale one when releasing without it, so
    // a no-handler hand-off (notBefore = None) is immediately claimable again.
    val eligibility = notBefore.fold(Updates.unset("nextEligibleAt"))(t =>
      Updates.set("nextEligibleAt", new java.util.Date(t.toEpochMilli)))
    val base = Updates.combine(
      Updates.set("state", TaskState.Waiting),
      Updates.unset("workerId"),
      Updates.unset("leaseExpiresAt"),
      eligibility
    )
    val update = error.fold(base)(e => Updates.combine(base, Updates.set("lastError", e)))
    Try(Await.result(c.updateOne(filter, update).toFuture(), 10.seconds))
      .recover { case exception => logger.warn(s"TaskQueue.release($id) failed: ${exception.getMessage}") }
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
        case exception: Throwable =>
          logger.warn(s"TaskQueue.reapExpiredLeases failed: ${exception.getMessage}")
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

  // Counts via the {state, submittedAt} index prefix, then filters taskType over
  // the bounded waiting set — cheap (waiting ≤ corpus). On a read failure report 0
  // so the reaper fails OPEN (enqueues its trickle) rather than wedging scrapes on
  // a transient Mongo blip.
  override def waitingCount(taskType: TaskType): Int = coll match {
    case None => 0
    case Some(c) =>
      Try(Await.result(
        c.countDocuments(Filters.and(
          Filters.eq("state", TaskState.Waiting),
          Filters.eq("taskType", taskType.name))).toFuture(),
        10.seconds)).toOption.fold(0)(_.toInt)
  }

  override def monitor(activeLimit: Int): QueueSnapshot = coll match {
    case None => QueueSnapshot(Map.empty, Seq.empty)
    case Some(c) =>
      val active = Try {
        Await.result(
          c.find(Filters.in("state", TaskState.Waiting, TaskState.WorkedOn))
            .sort(Indexes.ascending("submittedAt"))
            .limit(activeLimit)
            .toFuture(),
          10.seconds).map(toSummary)
      }.recover {
        case exception: Throwable =>
          logger.warn(s"TaskQueue.monitor failed: ${exception.getMessage}")
          Seq.empty[TaskSummary]
      }.getOrElse(Seq.empty)
      QueueSnapshot(countByState(), active)
  }

  /** Push: ring `onWaiting` on every freshly enqueued task. `enqueue` is the
   *  only path that inserts a document and it always inserts in `waiting`, so an
   *  insert event is exactly "new claimable work" — releases and reaps (updates)
   *  are left to the worker pool's own reaper-ring + idle backstop, which keeps a
   *  perpetually-failing task from waking the whole pool on every retry.
   *
   *  The stream carries a server-side `$match` on `operationType == insert`, so the
   *  primary never ships the claim/complete/release/reap events — the vast majority
   *  of writes to `tasks` — down this cursor. Each instance's doorbell then decodes
   *  only actual new work (in a prod sample, ~527 of 1811 task events/15min), not
   *  every lifecycle write it would immediately discard. The client-side INSERT
   *  guard below is kept as defence-in-depth. Mirrors
   *  [[services.movies.MovieRepository.watchUpserts]]: the driver auto-resumes across
   *  transient blips, and on a standalone (non-replica-set) Mongo the stream just
   *  errors out and the pool falls back to its backstop. */
  override def watchWaiting(onWaiting: () => Unit): Option[AutoCloseable] = coll.map { c =>
    val subRef = new AtomicReference[Subscription]()
    c.watch(Seq(Aggregates.filter(Filters.eq("operationType", "insert")))).subscribe(new Observer[ChangeStreamDocument[Document]] {
      override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
      override def onNext(change: ChangeStreamDocument[Document]): Unit =
        if (change.getOperationType == OperationType.INSERT)
          try onWaiting()
          catch { case exception: Throwable => logger.warn(s"Task queue doorbell ring failed: ${exception.getMessage}") }
      override def onError(e: Throwable): Unit =
        logger.warn(s"Task queue change stream ended (${e.getMessage}) — worker pool falls back to its idle backstop.")
      override def onComplete(): Unit = ()
    })
    logger.info("MongoTaskQueue: watching change stream to wake workers on new tasks.")
    new AutoCloseable { override def close(): Unit = Option(subRef.get()).foreach(_.unsubscribe()) }
  }

  private def toSummary(document: Document): TaskSummary = TaskSummary(
    id             = document.getString("_id"),
    taskType       = document.getString("taskType"),
    dedupKey       = document.getString("dedupKey"),
    state          = document.getString("state"),
    submittedAt    = Instant.ofEpochMilli(document.getDate("submittedAt").getTime),
    attempts       = document.getInteger("attempts", 0),
    workerId       = Option(document.getString("workerId")),
    leaseExpiresAt = Option(document.getDate("leaseExpiresAt")).map(d => Instant.ofEpochMilli(d.getTime)),
    lastError      = Option(document.getString("lastError"))
  )

  private def payloadDocument(payload: Map[String, String]): org.mongodb.scala.bson.BsonDocument = {
    val bd = new org.mongodb.scala.bson.BsonDocument()
    payload.foreach { case (k, v) => bd.put(k, BsonString(v)) }
    bd
  }

  private def toTask(document: Document): Task = {
    val payload = document.get("payload")
      .filter(_.isDocument)
      .map { v =>
        val document = v.asDocument()
        document.keySet().asScala.iterator.map(k => k -> document.getString(k).getValue).toMap
      }
      .getOrElse(Map.empty[String, String])
    Task(
      id       = document.getString("_id"),
      taskType = TaskType.byName(document.getString("taskType")).getOrElse(
        throw new IllegalStateException(s"Unknown taskType ${document.getString("taskType")}")),
      dedupKey = document.getString("dedupKey"),
      payload  = payload,
      attempts = document.getInteger("attempts", 0)
    )
  }
}

object MongoTaskQueue {
  /** `{w:1, j:false}` — ack on primary apply, no journal-fsync/majority wait. The
   *  queue is recoverable bookkeeping, so the durability trade buys back the
   *  per-op cost that dominated the shared-cpu Mongo's load. See the class document. */
  val QueueWriteConcern: WriteConcern = WriteConcern.W1.withJournal(false)
}
