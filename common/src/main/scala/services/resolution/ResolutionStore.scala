package services.resolution

import com.mongodb.client.model.{IndexOptions => JIndexOptions, ReplaceOptions}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Indexes}
import play.api.Logging

import java.time.{Clock, Instant}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * The durable boundary behind a [[ResolutionCache]]: a string `hintKey` → the
 * resolved value (a TMDB id, IMDb id, or a Filmweb/RT/Metacritic url), with a
 * 24h freshness window. The cache's Caffeine layer sits in front; this is what
 * a cold (just-restarted) process reads on a Caffeine miss, and what gives the
 * resolution its persistence across restarts and across the worker fleet.
 *
 * Only HITS are stored — a resolver that found nothing writes nothing, so an
 * unresolvable film re-resolves next cycle rather than being remembered as a
 * permanent miss.
 *
 * Both implementations honour the same freshness contract (a value older than
 * [[ResolutionStore.Ttl]] reads as absent), so the cache semantics are
 * identical with or without Mongo.
 */
trait ResolutionStore {
  /** The stored value for `hintKey`, or None if never stored or older than the TTL. */
  def get(hintKey: String): Option[String]

  /** Store (or refresh the timestamp of) `hintKey` → `value`. */
  def put(hintKey: String, value: String): Unit
}

object ResolutionStore {
  /** The shared 24h freshness window for every resolution store — both the
   *  read-side expiry filter and the Mongo TTL index derive from it. */
  val Ttl: FiniteDuration = 24.hours
}

/** In-memory `ResolutionStore` for tests and Mongo-less local dev. Honours the
 *  same TTL expiry as the Mongo store via an injectable clock. */
class InMemoryResolutionStore(clock: Clock = Clock.systemUTC()) extends ResolutionStore {
  private val entries = new ConcurrentHashMap[String, (String, Instant)]()

  override def get(hintKey: String): Option[String] =
    Option(entries.get(hintKey)).collect {
      case (value, at) if at.isAfter(clock.instant().minusMillis(ResolutionStore.Ttl.toMillis)) => value
    }

  override def put(hintKey: String, value: String): Unit = {
    entries.put(hintKey, (value, clock.instant())); ()
  }
}

/**
 * Mongo-backed `ResolutionStore`. Each source gets its OWN collection
 * (`resolve_tmdb`, `resolve_imdb`, `resolve_filmweb`, `resolve_rt`,
 * `resolve_mc`), documents `{ _id: <hintKey>, value: <String>, at: ISODate }`.
 *
 * Expiry is enforced two ways that agree: a Mongo TTL index on `at` reaps old
 * docs in the background, and `get` filters `at >= now - Ttl` so an expired doc
 * the reaper hasn't swept yet (its cadence is ~60s) still reads as absent —
 * which keeps the in-memory fake's semantics exact. Relaxed `{w:1,j:false}`
 * write concern (like the freshness store): a resolution result is a cache, so
 * a lost write just costs a re-resolution.
 *
 * A `null` db disables persistence — `get` always misses and `put` is a no-op,
 * so the in-front Caffeine layer still works, it just doesn't survive a restart.
 */
class MongoResolutionStore(
  db:             Option[MongoDatabase],
  collectionName: String,
  clock:          Clock = Clock.systemUTC()
) extends ResolutionStore with Logging {

  private val coll: Option[MongoCollection[Document]] =
    db.map(_.getCollection(collectionName).withWriteConcern(services.tasks.MongoTaskQueue.QueueWriteConcern))

  // Build the TTL index off-thread so construction never blocks worker boot on Mongo.
  coll.foreach { c =>
    val thread = new Thread(() => ensureTtlIndex(c), s"$collectionName-init")
    thread.setDaemon(true)
    thread.start()
  }

  override def get(hintKey: String): Option[String] =
    coll.flatMap { c =>
      val cutoff = new java.util.Date(clock.instant().minusMillis(ResolutionStore.Ttl.toMillis).toEpochMilli)
      Try(Await.result(c.find(Filters.and(Filters.eq("_id", hintKey), Filters.gte("at", cutoff))).headOption(), 5.seconds))
        .toOption.flatten
        .flatMap(document => Option(document.getString("value")))
    }

  override def put(hintKey: String, value: String): Unit = coll.foreach { c =>
    // Best-effort, fire-and-forget: a write failure must never break the
    // caller's resolution loop — the value is already in the in-front cache.
    Try {
      c.replaceOne(
        Filters.eq("_id", hintKey),
        Document("_id" -> hintKey, "value" -> value, "at" -> new java.util.Date(clock.instant().toEpochMilli)),
        new ReplaceOptions().upsert(true)
      ).subscribe(
        (_: org.mongodb.scala.result.UpdateResult) => (),
        (exception: Throwable) => logger.debug(s"Resolution write failed for $collectionName/$hintKey: ${exception.getMessage}")
      )
    }.recover { case exception => logger.debug(s"Resolution write failed for $collectionName/$hintKey: ${exception.getMessage}") }
  }

  /** Create the TTL index on `at`, and bring an existing one's expiry in line
   *  with `Ttl` via `collMod` (createIndex can create but never ALTER a TTL).
   *  Best-effort, mirroring `UptimeMonitor.ensureIndexes`. */
  private def ensureTtlIndex(c: MongoCollection[Document]): Unit = {
    val ttlSeconds = ResolutionStore.Ttl.toSeconds
    Try {
      Await.result(c.createIndex(
        Indexes.ascending("at"),
        new JIndexOptions().expireAfter(ttlSeconds, TimeUnit.SECONDS)
      ).toFuture(), 10.seconds)
    }.recover { case exception => logger.debug(s"$collectionName TTL index not (re)created — collMod will reconcile: ${exception.getMessage}") }

    db.foreach { database =>
      Try {
        val collMod = new org.bson.Document("collMod", collectionName)
          .append("index", new org.bson.Document("keyPattern", new org.bson.Document("at", 1))
            .append("expireAfterSeconds", ttlSeconds))
        Await.result(database.runCommand(collMod).toFuture(), 10.seconds)
      }.recover { case exception => logger.debug(s"$collectionName TTL collMod skipped: ${exception.getMessage}") }
    }
  }
}
