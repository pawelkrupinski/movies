package services.tasks

import java.time.Instant
import java.util.UUID

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

import com.mongodb.client.model.IndexOptions as JIndexOptions
import com.mongodb.{MongoWriteException, WriteConcern}
import org.mongodb.scala.bson.BsonString
import org.mongodb.scala.model.{Accumulators, Aggregates, Filters, FindOneAndUpdateOptions, Indexes, ReturnDocument, Updates}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import play.api.Logging
import services.movies.ChangeStreamReopen
import services.tasktypes.TaskType

/**
 * Mongo-backed `TaskQueue`, collection `tasks`. Document shape:
 * {{{
 *   { _id: <uuid>, taskType: "ScrapeCinema", dedupKey: "scrape|kino-x",
 *     payload: { ... },              // string→string sub-document
 *     state: "waiting"|"worked_on",  // `complete` deletes the document — no tombstone
 *     active: Bool,                  // always true now (drives the dedup index)
 *     submittedAt: ISODate,          // claim order: enqueue time, earlier by `claimAhead`
 *     enqueuedAt: ISODate,           // the real enqueue time (absent on pre-2026-09-29 rows)
 *     attempts: Int,
 *     workerId: String?, leaseExpiresAt: ISODate? }
 * }}}
 *
 * Idempotent enqueue and atomic claim are expressed declaratively through
 * indexes and `findOneAndUpdate`. The lease cycle's rules (claim order, attempts,
 * the backoff hold, the ownership guard, lease expiry) are [[TaskLeases]]', shared
 * with [[InMemoryTaskQueue]]; this class translates them to Mongo ([[MongoTaskRows]]).
 * The redundancy/skip decision lives in handlers, above this boundary.
 *
 * Dedup uses a **unique partial index** on `dedupKey` over `active: true`.
 * Partial filters only allow equality/$exists (not `$in`), hence the explicit
 * `active` flag rather than a `state ∈ {waiting,worked_on}` predicate. `complete`
 * deletes the document, so a finished key has no row in the index and re-enqueuing
 * it is allowed; concurrent inserts race on the index and the loser's duplicate-key
 * error is caught as `Duplicate` (see [[tools.MongoErrors]]).
 *
 * Blocking `.toFuture()` throughout — callers are daemon worker/reaper threads.
 *
 * Writes use a relaxed `{w:1, j:false}` write concern ([[tools.MongoWriteConcerns.Bookkeeping]]):
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
class MongoTaskQueue(db: Option[MongoDatabase] = None, collectionName: String = "tasks") extends LeasedTaskQueue with Logging {
  import MongoTaskQueue.*

  private val coll: Option[MongoCollection[Document]] =
    db.map(_.getCollection(collectionName).withWriteConcern(tools.MongoWriteConcerns.Bookkeeping))

  /** The effective write concern of the `tasks` collection — for diagnostics/tests. */
  def collectionWriteConcern: Option[WriteConcern] = coll.map(_.writeConcern)

  for (database <- db; c <- coll) {
    val thread = new Thread(() => createIndexes(database, c), "tasks-init")
    thread.setDaemon(true)
    thread.start()
  }

  private def createIndexes(database: MongoDatabase, c: MongoCollection[Document]): Unit = Try {
    tools.MongoIndex.ensure(database, collectionName, Indexes.ascending("dedupKey"),
      new JIndexOptions().unique(true).partialFilterExpression(Filters.eq("active", true)), "Task queue")
    Await.result(c.createIndex(Indexes.compoundIndex(Indexes.ascending("state"), Indexes.ascending("submittedAt"))).toFuture(), 10.seconds)
    Await.result(c.createIndex(Indexes.compoundIndex(Indexes.ascending("state"), Indexes.ascending("leaseExpiresAt"))).toFuture(), 10.seconds)
  }.recover { case exception => logger.warn(s"Task queue index creation failed: ${exception.getMessage}") }

  override def enqueue(
    taskType:    TaskType,
    dedupKey:    String,
    payload:     Map[String, String],
    submittedAt: Instant,
    notBefore:   Option[Instant],
    claimAhead:  FiniteDuration
  ): EnqueueResult = coll match {
    case None => EnqueueResult.Duplicate
    case Some(c) =>
      val filter = Filters.and(Filters.eq("dedupKey", dedupKey), Filters.eq("active", true))
      val onInsert = Updates.combine((Updates.setOnInsert("_id", UUID.randomUUID().toString) +:
        inserted(taskType, dedupKey, payload, submittedAt, notBefore, claimAhead).toSeq.map { case (field, value) =>
          Updates.setOnInsert(field, value) })*)
      Try {
        val res = tools.MongoWriteErrors.counted(c)(Await.result(c.updateOne(filter, onInsert, new com.mongodb.client.model.UpdateOptions().upsert(true)).toFuture(), 10.seconds))
        if (res.getUpsertedId != null) EnqueueResult.Added else EnqueueResult.Duplicate
      }.recover {
        // Losing the race on the unique index IS the dedup working: another writer
        // got the same key in first. Anything else is Mongo failing to answer, and
        // must say so — reported as `Duplicate` it read as "already queued" and the
        // task was silently lost.
        case exception: MongoWriteException if tools.MongoErrors.isDuplicateKey(exception) => EnqueueResult.Duplicate
        case exception =>
          logger.warn(s"TaskQueue.enqueue($dedupKey) failed: ${exception.getMessage}")
          EnqueueResult.Failed(exception.getMessage)
      }.get
  }

  override def raise(dedupKey: String, claimFrom: Instant): Boolean = coll match {
    case None => false
    case Some(c) =>
      val filter = Filters.and(Filters.eq("dedupKey", dedupKey), Filters.eq("active", true), Filters.eq("state", TaskState.Waiting))
      val at     = org.mongodb.scala.bson.BsonDateTime(claimFrom.toEpochMilli)
      // $min: forward only -- a task already ahead of `claimFrom` keeps its place; an eligibility set later is brought
      // to `claimFrom`, none set (eligible already) stays none ($min would set a missing field)
      Try {
        val matched = Await.result(c.updateOne(filter, Updates.min("submittedAt", at)).toFuture(), 10.seconds).getMatchedCount > 0
        if (matched) Await.result(c.updateOne(Filters.and(filter, Filters.gt("nextEligibleAt", at)), Updates.set("nextEligibleAt", at)).toFuture(), 10.seconds)
        matched
      }.recover {
        case exception =>
          logger.warn(s"TaskQueue.raise($dedupKey) failed: ${exception.getMessage}")
          false
      }.get
  }

  override def amendWaiting(dedupKey: String, fields: Map[String, String]): Boolean = coll match {
    case None => false
    case Some(_) if fields.isEmpty => false
    case Some(c) =>
      // Conditional on `state` in the filter itself, so a claim racing this update either
      // sees the amended payload or leaves this matching nothing — never a half-read one.
      val filter = Filters.and(Filters.eq("dedupKey", dedupKey), Filters.eq("active", true),
        Filters.eq("state", TaskState.Waiting))
      val update = Updates.combine(fields.toSeq.map { case (k, v) => Updates.set(s"payload.$k", v) }*)
      Try(Await.result(c.updateOne(filter, update).toFuture(), 10.seconds).getMatchedCount > 0).recover {
        case exception =>
          logger.warn(s"TaskQueue.amendWaiting($dedupKey) failed: ${exception.getMessage}")
          false
      }.get
  }

  // The lease cycle's rules are [[TaskLeases]]' ([[LeasedTaskQueue]]), shared with [[InMemoryTaskQueue]]; this queue
  // supplies only where the rows live ([[MongoTaskRows]]). Claiming is atomic and multi-machine-safe through
  // `findOneAndUpdate` on the primary ([[MongoTaskRows.claimOldest]]), not through the write concern; completing
  // DELETES the document (the dedup index is partial on `active:true`, so a gone doc allows re-enqueue), leaving `tasks`
  // only the few active tasks and one change-stream event per completion.
  protected val taskRows: TaskRows = coll.fold(TaskRows.absent)(new MongoTaskRows(_))

  override def reapExpiredLeases(now: Instant): Int = coll match {
    case None => 0
    case Some(c) =>
      // A failed reap THROWS (after the retired-type sweep, which stands alone): answered as 0 it read as nothing
      // expired, and the lease reaper's tick completed while no lease came back.
      val reaped = Try(super.reapExpiredLeases(now))
      dropRetiredTypes(c, now)
      reaped.get
  }

  /** The same pass's other sweep: a row of a type this build does not know, waiting past
   *  [[MongoTaskQueue.RetiredTaskTypeAfter]], is a RETIRED type's — no image will claim it — so it is
   *  deleted, logged by type. A younger one is left for the newer image a rollback will hand back to.
   *  Aged by `enqueuedAt` (the real enqueue time), `submittedAt` on a row from before that field. */
  private def dropRetiredTypes(c: MongoCollection[Document], now: Instant): Unit = {
    val cutoff  = new java.util.Date(now.minus(MongoTaskQueue.RetiredTaskTypeAfter).toEpochMilli)
    val retired = Filters.and(
      Filters.eq("state", TaskState.Waiting),
      Filters.nin("taskType", MongoTaskQueue.KnownTypeNames*),
      Filters.or(
        Filters.lt("enqueuedAt", cutoff),
        Filters.and(Filters.exists("enqueuedAt", false), Filters.lt("submittedAt", cutoff))))
    Try {
      val rows = Await.result(c.find(retired).projection(org.mongodb.scala.model.Projections.include("taskType"))
        .limit(MongoTaskQueue.RetiredDropsPerPass).batchSize(tools.MongoReplies.Default).toFuture(), 10.seconds)
      if (rows.nonEmpty) {
        // Deleted by id AND the same filter, so a row a newer image claimed meanwhile is left alone.
        Await.result(c.deleteMany(Filters.and(Filters.in("_id", rows.map(_.getString("_id"))*), retired)).toFuture(), 10.seconds)
        rows.groupBy(_.getString("taskType")).toSeq.sortBy(_._1).foreach { case (taskType, ofType) =>
          logger.warn(s"TaskQueue dropped ${ofType.size} waiting task(s) of retired type $taskType, unknown to this build " +
            s"and queued over ${MongoTaskQueue.RetiredTaskTypeAfter.toDays} days ago.")
        }
      }
    }.recover { case exception => logger.warn(s"TaskQueue retired-type sweep failed: ${exception.getMessage}") }
    ()
  }

  // Counts via the {state, submittedAt} index prefix, then filters taskType over
  // the bounded waiting set — cheap (waiting ≤ corpus). A read failure PROPAGATES:
  // it used to answer 0, which is the reading of an empty queue, so the one moment
  // the backlog was unknown was the moment the reaper admitted its whole budget on
  // top of it. The caller decides what an unknown backlog means (see the trait).
  override def waitingCount(taskType: TaskType): Int = coll match {
    case None => 0
    case Some(c) =>
      Await.result(
        c.countDocuments(Filters.and(
          Filters.eq("state", TaskState.Waiting),
          Filters.eq("taskType", taskType.name))).toFuture(),
        10.seconds).toInt
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
   *  guard in [[TaskInsertStream]] is kept as defence-in-depth. The subscription
   *  itself — and its reopen after a terminal error, which is what keeps this node's
   *  workers woken across a Mongo blip — lives in [[TaskInsertStream]]. */
  override def watchWaiting(onWaiting: Option[Instant] => Unit): Option[AutoCloseable] = coll.map { c =>
    val stream = new TaskInsertStream(
      open      = c.watch(Seq(Aggregates.filter(Filters.eq("operationType", "insert")))).subscribe(_),
      onWaiting = onWaiting,
      schedule  = new ChangeStreamReopen.DaemonSchedule("MongoTaskQueue"))
    stream.start()
    logger.info("MongoTaskQueue: watching change stream to wake workers on new tasks.")
    stream
  }
}

/** The `tasks` collection as [[TaskRows]]: each [[TaskRowMatch]] a filter, each [[TaskRowChange]] an
 *  update. Blocking, like the queue; every call throws on a failed read or write. */
private[tasks] final class MongoTaskRows(c: MongoCollection[Document]) extends TaskRows {
  import MongoTaskQueue.*

  def claimOldest(matching: TaskRowMatch, change: TaskRowChange): Option[Task] = {
    val opts = FindOneAndUpdateOptions().sort(Indexes.ascending("submittedAt")).returnDocument(ReturnDocument.AFTER)
    tools.MongoWriteErrors.counted(c)(Await.result(c.findOneAndUpdate(filter(matching), update(change), opts).headOption(), 10.seconds))
      .flatMap(d => TaskType.byName(d.getString("taskType")).map(toTask(d, _)))
  }

  def updateOne(matching: TaskRowMatch, change: TaskRowChange): Unit = {
    Await.result(c.updateOne(filter(matching), update(change)).toFuture(), 10.seconds); ()
  }

  def updateAll(matching: TaskRowMatch, change: TaskRowChange): Int =
    Await.result(c.updateMany(filter(matching), update(change)).toFuture(), 10.seconds).getModifiedCount.toInt

  def deleteOne(matching: TaskRowMatch): Unit = {
    Await.result(c.deleteOne(filter(matching)).toFuture(), 10.seconds); ()
  }

  /** One `$group` over the matching rows instead of a `countDocuments` per state: the counts run on the 60s
   *  heartbeat AND on every `/debug` request and metrics scrape, so halving their round-trips is worth it. */
  def countByState(matching: TaskRowMatch): Map[String, Long] =
    Await.result(c.aggregate(Seq(Aggregates.filter(filter(matching)), Aggregates.group("$state", Accumulators.sum("n", 1))))
      .batchSize(tools.MongoReplies.Default).toFuture(), 10.seconds)
      .flatMap { doc =>
        for {
          state <- doc.get("_id").filter(_.isString).map(_.asString().getValue)
          count <- doc.get("n").filter(_.isNumber).map(_.asNumber().longValue())
        } yield state -> count
      }.toMap

  // Served by the `{state, submittedAt}` index for a one-state match (the snapshot's walk): a single mixed-direction
  // sort over every active state would make Mongo blocking-sort the whole active set in memory on every 5s poll.
  def oldest(matching: TaskRowMatch, limit: Int): Seq[TaskSummary] =
    Await.result(c.find(filter(matching)).sort(Indexes.ascending("submittedAt")).limit(limit)
      .batchSize(tools.MongoReplies.Default).toFuture(), 10.seconds).map(toSummary)
}

object MongoTaskQueue {
  private[tasks] def toSummary(document: Document): TaskSummary = TaskSummary(
    id             = document.getString("_id"),
    taskType       = document.getString("taskType"),
    dedupKey       = document.getString("dedupKey"),
    state          = document.getString("state"),
    submittedAt    = Instant.ofEpochMilli(document.getDate("submittedAt").getTime),
    attempts       = document.getInteger("attempts", 0),
    workerId       = Option(document.getString("workerId")),
    leaseExpiresAt = Option(document.getDate("leaseExpiresAt")).map(d => Instant.ofEpochMilli(d.getTime)),
    lastError      = Option(document.getString("lastError")),
    nextEligibleAt = Option(document.getDate("nextEligibleAt")).map(d => Instant.ofEpochMilli(d.getTime)),
    enqueuedAt     = Option(document.getDate("enqueuedAt")).map(d => Instant.ofEpochMilli(d.getTime))
  )

  private[tasks] def toTask(document: Document, taskType: TaskType): Task =
    Task(
      id       = document.getString("_id"),
      taskType = taskType,
      dedupKey = document.getString("dedupKey"),
      payload  = payloadOf(document),
      attempts = document.getInteger("attempts", 0)
    )

  /** A row filter: every condition [[TaskRowMatch]] gives. */
  private[tasks] def filter(matching: TaskRowMatch): org.mongodb.scala.bson.conversions.Bson = {
    def at(instant: Instant) = new java.util.Date(instant.toEpochMilli)
    Filters.and((
      matching.id.map(Filters.eq("_id", _)).toSeq ++
      matching.state.map(Filters.eq("state", _)) ++
      Option.when(matching.states.nonEmpty)(Filters.in("state", matching.states*)) ++
      matching.workerId.map(Filters.eq("workerId", _)) ++
      Option.when(matching.knownTypesOnly)(Filters.in("taskType", KnownTypeNames*)) ++
      matching.eligibleAt.map(now => Filters.or(Filters.exists("nextEligibleAt", false), Filters.lte("nextEligibleAt", at(now)))) ++
      matching.leaseExpiredBefore.map(now => Filters.lt("leaseExpiresAt", at(now))))*)
  }

  /** A row update: what [[TaskRowChange]] says, every other field left alone. */
  private[tasks] def update(change: TaskRowChange): org.mongodb.scala.bson.conversions.Bson = {
    def at(instant: Instant) = new java.util.Date(instant.toEpochMilli)
    val lease = change.lease.fold(Seq(Updates.unset("workerId"), Updates.unset("leaseExpiresAt")))(held =>
      Seq(Updates.set("workerId", held.workerId), Updates.set("leaseExpiresAt", at(held.expiresAt))))
    Updates.combine((
      Seq(Updates.set("state", change.state)) ++ lease ++
      change.nextEligibleAt.map(_.fold(Updates.unset("nextEligibleAt"))(t => Updates.set("nextEligibleAt", at(t)))) ++
      Option.when(change.attemptsBy != 0)(Updates.inc("attempts", change.attemptsBy)) ++
      change.lastError.map(Updates.set("lastError", _)))*)
  }

  /** A new task's fields, `_id` aside, as an enqueue inserts them. A `notBefore` holds the task back from `claim` until
   *  that instant — the same `nextEligibleAt` gate the release-backoff uses (see `claim`'s `eligible`). */
  private[tasks] def inserted(taskType: TaskType, dedupKey: String, payload: Map[String, String], submittedAt: Instant,
                              notBefore: Option[Instant], claimAhead: FiniteDuration): Document =
    Document(
      "taskType"    -> BsonString(taskType.name),
      "dedupKey"    -> BsonString(dedupKey),
      "payload"     -> payloadDocument(payload),
      "state"       -> BsonString(TaskState.Waiting),
      "active"      -> org.mongodb.scala.bson.BsonBoolean(true),
      "submittedAt" -> org.mongodb.scala.bson.BsonDateTime(submittedAt.toEpochMilli - claimAhead.toMillis),
      "enqueuedAt"  -> org.mongodb.scala.bson.BsonDateTime(submittedAt.toEpochMilli),
      "attempts"    -> org.mongodb.scala.bson.BsonInt32(0)) ++
      notBefore.fold(Document())(t =>
        Document("nextEligibleAt" -> org.mongodb.scala.bson.BsonDateTime(t.toEpochMilli)))

  /** A payload as stored: a sub-document whose field names are its keys. */
  private[tasks] def payloadDocument(payload: Map[String, String]): org.mongodb.scala.bson.BsonDocument = {
    val bd = new org.mongodb.scala.bson.BsonDocument()
    payload.foreach { case (k, v) => bd.put(k, BsonString(v)) }
    bd
  }

  /** The payload a task's document holds; none without one. */
  private[tasks] def payloadOf(document: Document): Map[String, String] =
    document.get("payload")
      .filter(_.isDocument)
      .map { v =>
        val payload = v.asDocument()
        payload.keySet().asScala.iterator.map(k => k -> payload.getString(k).getValue).toMap
      }
      .getOrElse(Map.empty[String, String])

  /** How long a task of a type this build does not know waits before the lease reaper drops it as RETIRED. Such a
   *  row is, while a rollback lasts, a newer image's work this one must leave for it; no rollback runs a week (an
   *  image older than that is a re-deploy, not a rollback), so a row still unknown after it belongs to a type no
   *  image handles any more and would otherwise wait for ever. */
  val RetiredTaskTypeAfter: java.time.Duration = java.time.Duration.ofDays(7)

  /** At most this many retired rows per reaper pass — a bound on one pass's read, not a budget: the next drops the rest. */
  private val RetiredDropsPerPass = 500

  /** The task types this build has a decoder for — what `claim`, the counts and the snapshot restrict their match
   *  to. */
  private[tasks] val KnownTypeNames: Seq[String] = TaskType.all.map(_.name)
}
