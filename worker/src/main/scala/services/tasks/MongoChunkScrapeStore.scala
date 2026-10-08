package services.tasks

import com.mongodb.MongoWriteException
import com.mongodb.client.model.{IndexOptions => JIndexOptions}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.bson.{BsonArray, BsonString}
import org.mongodb.scala.model.{Filters, FindOneAndReplaceOptions, Indexes, ReturnDocument, Updates}
import play.api.Logging

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Mongo-backed [[ChunkScrapeStore]] (write concern `{w:1, j:false}`, like the
 * task queue — recoverable bookkeeping, not a system of record). Two collections:
 *
 *  - `scrape_runs`: ONE doc per cinema (`_id = cinema displayName`) — the
 *    per-cinema mutex + active runId + expectedKeys + createdAt. `startRun`
 *    inserts (loser of the unique `_id` race = a fresh run already active → None)
 *    or, on conflict, atomically replaces a STALE run via a `createdAt < threshold`
 *    guard (supersession). TTL on `createdAt` clears a wedged run.
 *  - `scrape_chunks`: one doc per `(cinema, runId, key)` (`_id` is their join) —
 *    the stored slice, whether it read whole, and the attempt that read it. Upsert =
 *    idempotent on chunk retry, never over a later attempt's. TTL on `storedAt`.
 */
class MongoChunkScrapeStore(db: Option[MongoDatabase] = None) extends ChunkScrapeStore with Logging {
  import MongoChunkScrapeStore._

  private val runs: Option[MongoCollection[Document]] =
    db.map(_.getCollection("scrape_runs").withWriteConcern(MongoTaskQueue.QueueWriteConcern))
  private val chunks: Option[MongoCollection[Document]] =
    db.map(_.getCollection("scrape_chunks").withWriteConcern(MongoTaskQueue.QueueWriteConcern))

  runs.foreach { c =>
    val t = new Thread(() => createIndexes(c, chunks), "scrape-runs-init"); t.setDaemon(true); t.start()
  }

  private def createIndexes(r: MongoCollection[Document], ch: Option[MongoCollection[Document]]): Unit = Try {
    Await.result(r.createIndex(Indexes.ascending("createdAt"),
      new JIndexOptions().expireAfter(TtlHours, TimeUnit.HOURS)).toFuture(), 10.seconds)
    ch.foreach { c =>
      Await.result(c.createIndex(Indexes.compoundIndex(Indexes.ascending("cinema"), Indexes.ascending("runId"))).toFuture(), 10.seconds)
      Await.result(c.createIndex(Indexes.ascending("storedAt"),
        new JIndexOptions().expireAfter(TtlHours, TimeUnit.HOURS)).toFuture(), 10.seconds)
    }
  }.recover { case e => logger.warn(s"scrape-run index creation failed: ${e.getMessage}") }

  def startRun(cinema: String, expectedKeys: Seq[String], now: Instant, staleAfter: FiniteDuration): Option[String] =
    runs.flatMap { c =>
      val runId = java.util.UUID.randomUUID().toString
      val doc = runDoc(cinema, runId, expectedKeys, now)
      // 1) Try to claim by inserting the per-cinema doc. Winning the unique-_id
      //    race = no active run existed.
      val inserted = Try { Await.result(c.insertOne(doc).toFuture(), 10.seconds); true }.recover {
        case e: MongoWriteException if services.MongoErrors.isDuplicateKey(e) => false
        case e: Throwable => logger.warn(s"startRun insert for $cinema failed: ${e.getMessage}"); false
      }.getOrElse(false)
      if (inserted) Some(runId)
      else {
        // 2) A doc exists. Replace it ONLY if it's stale (abandoned) — supersede.
        val staleThreshold = new java.util.Date(now.minusMillis(staleAfter.toMillis).toEpochMilli)
        val filter = Filters.and(Filters.eq("_id", cinema), Filters.lt("createdAt", staleThreshold))
        Try {
          Await.result(c.findOneAndReplace(filter, doc,
            FindOneAndReplaceOptions().returnDocument(ReturnDocument.AFTER)).headOption(), 10.seconds)
        }.recover { case e => logger.warn(s"startRun replace for $cinema failed: ${e.getMessage}"); None }
          .getOrElse(None)
          .map(_ => runId)
      }
    }

  // READS PROPAGATE a failure. Each answered "nothing" before, and each "nothing" is acted
  // on: no active run reads as superseded (the chunk is skipped) or as room for a new run;
  // no stored chunks let the reduce publish an empty/partial listing and then complete the
  // run, deleting the chunks it never read. A throw fails the task, which retries.
  def activeRun(cinema: String): Option[ChunkRun] = runs.flatMap { c =>
    Await.result(c.find(Filters.eq("_id", cinema)).headOption(), 10.seconds).map(toRun)
  }

  // A failed write THROWS too: logged and dropped, a lost INCOMPLETE marker let the run reduce as a
  // whole listing and prune what the failed read missed. The throw fails the task, which retries.
  // [[StoredChunk.replaces]] as a filter: the upsert matches the key only while no LATER attempt holds
  // it (a doc without an attempt predates them: attempt 0), and one that does turns the upsert's insert
  // into a duplicate key — that attempt's slice stands.
  def storeChunk(cinema: String, runId: String, key: String, chunk: StoredChunk, now: Instant): Unit = chunks.foreach { c =>
    val id = chunkId(cinema, runId, key)
    val update = Updates.combine(
      Updates.setOnInsert("_id", id),
      Updates.set("cinema", cinema), Updates.set("runId", runId), Updates.set("key", key),
      Updates.set("value", chunk.valueJson), Updates.set("complete", chunk.complete), Updates.set("attempt", chunk.attempt),
      Updates.set("storedAt", new java.util.Date(now.toEpochMilli)))
    try {
      val _ = Await.result(c.updateOne(Filters.and(Filters.eq("_id", id), Filters.not(Filters.gt("attempt", chunk.attempt))), update,
        new com.mongodb.client.model.UpdateOptions().upsert(true)).toFuture(), 10.seconds)
    } catch {
      case e: MongoWriteException if services.MongoErrors.isDuplicateKey(e) =>
        logger.info(s"storeChunk($cinema/$runId/$key) attempt ${chunk.attempt} kept out: a later attempt stored it")
    }
  }

  // Only the keys: asked on every chunk that lands, reading each stored chunk's parse (~7 KB) as well
  // made a run's completion checks quadratic in its chunks.
  def storedKeys(cinema: String, runId: String): Set[String] = chunks.toSeq.flatMap { c =>
    Await.result(c.find(Filters.and(Filters.eq("cinema", cinema), Filters.eq("runId", runId)))
      .projection(org.mongodb.scala.model.Projections.include("key"))
      .batchSize(tools.MongoReplies.Default).toFuture(), 10.seconds)
  }.map(_.getString("key")).toSet

  def loadChunks(cinema: String, runId: String): Map[String, StoredChunk] =
    loadDocs(cinema, runId).map(d => d.getString("key") -> StoredChunk(
      valueJson = d.getString("value"),
      complete  = d.get("complete").filter(_.isBoolean).forall(_.asBoolean().getValue),
      attempt   = d.get("attempt").filter(_.isInt32).fold(0)(_.asInt32().getValue))).toMap

  private def loadDocs(cinema: String, runId: String): Seq[Document] = chunks.toSeq.flatMap { c =>
    Await.result(c.find(Filters.and(Filters.eq("cinema", cinema), Filters.eq("runId", runId))).batchSize(tools.MongoReplies.Default).toFuture(), 10.seconds)
  }

  def activeRuns(): Seq[ChunkRun] = runs.toSeq.flatMap { c =>
    Await.result(c.find().batchSize(tools.MongoReplies.Default).toFuture(), 10.seconds).map(toRun)
  }

  def completeRun(cinema: String, runId: String): Unit = {
    runs.foreach(c => Try(Await.result(c.deleteOne(Filters.and(Filters.eq("_id", cinema), Filters.eq("runId", runId))).toFuture(), 10.seconds)))
    chunks.foreach(c => Try(Await.result(c.deleteMany(Filters.and(Filters.eq("cinema", cinema), Filters.eq("runId", runId))).toFuture(), 10.seconds)))
  }

  private def runDoc(cinema: String, runId: String, expectedKeys: Seq[String], now: Instant): Document =
    Document("_id" -> cinema, "runId" -> runId,
      "expectedKeys" -> BsonArray.fromIterable(expectedKeys.map(BsonString(_))),
      "createdAt" -> new java.util.Date(now.toEpochMilli))

  private def toRun(d: Document): ChunkRun = ChunkRun(
    cinema       = d.getString("_id"),
    runId        = d.getString("runId"),
    expectedKeys = d.get("expectedKeys").filter(_.isArray)
      .map(_.asArray().getValues.asScala.iterator.map(_.asString().getValue).toVector).getOrElse(Vector.empty),
    createdAt    = Instant.ofEpochMilli(d.getDate("createdAt").getTime))
}

object MongoChunkScrapeStore {
  private val TtlHours = 1L
  private def chunkId(cinema: String, runId: String, key: String): String = s"$cinema|$runId|$key"
}
