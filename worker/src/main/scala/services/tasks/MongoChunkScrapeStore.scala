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
 *    the stored slice. Upsert = idempotent on chunk retry. TTL on `storedAt`.
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

  def activeRun(cinema: String): Option[ChunkRun] = runs.flatMap { c =>
    Try(Await.result(c.find(Filters.eq("_id", cinema)).headOption(), 10.seconds)).toOption.flatten.map(toRun)
  }

  def storeChunk(cinema: String, runId: String, key: String, valueJson: String, now: Instant): Unit = chunks.foreach { c =>
    val id = chunkId(cinema, runId, key)
    val update = Updates.combine(
      Updates.setOnInsert("_id", id),
      Updates.set("cinema", cinema), Updates.set("runId", runId), Updates.set("key", key),
      Updates.set("value", valueJson), Updates.set("storedAt", new java.util.Date(now.toEpochMilli)))
    Try(Await.result(c.updateOne(Filters.eq("_id", id), update,
      new com.mongodb.client.model.UpdateOptions().upsert(true)).toFuture(), 10.seconds))
      .recover { case e => logger.warn(s"storeChunk($cinema/$runId/$key) failed: ${e.getMessage}") }
  }

  def storedKeys(cinema: String, runId: String): Set[String] = loadDocs(cinema, runId).map(_.getString("key")).toSet

  def loadChunks(cinema: String, runId: String): Map[String, String] =
    loadDocs(cinema, runId).map(d => d.getString("key") -> d.getString("value")).toMap

  private def loadDocs(cinema: String, runId: String): Seq[Document] = chunks.toSeq.flatMap { c =>
    Try(Await.result(c.find(Filters.and(Filters.eq("cinema", cinema), Filters.eq("runId", runId))).toFuture(), 10.seconds))
      .getOrElse(Seq.empty)
  }

  def activeRuns(): Seq[ChunkRun] = runs.toSeq.flatMap { c =>
    Try(Await.result(c.find().toFuture(), 10.seconds).map(toRun)).getOrElse(Seq.empty)
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
