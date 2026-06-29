package services.enrichment

import com.mongodb.client.model.ReplaceOptions
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Indexes}
import com.mongodb.client.model.{IndexOptions => JIndexOptions}
import play.api.Logging

import java.time.{Clock, Instant}
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/** A recorded OMDb resolution attempt for a film — when it last ran, and how many
 *  consecutive MISSES so far (the exponential-backoff level). */
case class OmdbAttempt(at: Instant, level: Int)

/**
 * Durable per-film backoff for the OMDb identifier sweep ([[OmdbBackfill]]). OMDb
 * data for a film barely changes, so a film OMDb couldn't resolve must NOT be
 * re-probed on every daily sweep — that just burns the free 1000/day quota. We
 * remember each MISS (timestamp + a doubling level) and skip the film until its
 * window elapses. A HIT writes nothing here: the film gains the identifier and so
 * stops being a candidate on its own.
 */
trait OmdbAttemptStore {
  def get(filmKey: String): Option[OmdbAttempt]
  def record(filmKey: String, level: Int, at: Instant): Unit
}

object OmdbAttemptStore {
  /** No-op store — every film is always eligible (no backoff). Default for tests
   *  and Mongo-less wiring. */
  val noop: OmdbAttemptStore = new OmdbAttemptStore {
    def get(filmKey: String): Option[OmdbAttempt] = None
    def record(filmKey: String, level: Int, at: Instant): Unit = ()
  }
}

/** In-memory store for tests / Mongo-less local dev. */
class InMemoryOmdbAttemptStore extends OmdbAttemptStore {
  private val entries = new ConcurrentHashMap[String, OmdbAttempt]()
  def get(filmKey: String): Option[OmdbAttempt] = Option(entries.get(filmKey))
  def record(filmKey: String, level: Int, at: Instant): Unit = { entries.put(filmKey, OmdbAttempt(at, level)); () }
}

/**
 * Mongo-backed store: docs `{ _id: <filmKey>, at: ISODate, level: Int }` in the
 * `omdb_attempts` collection. Relaxed `{w:1,j:false}` write concern + fire-and-
 * forget writes (a lost backoff stamp just costs one extra OMDb probe). A 90-day
 * TTL on `at` — well beyond the 30-day backoff cap — reaps stamps for films that
 * left the corpus, so the collection can't grow unbounded. A `null` db disables
 * persistence (every film eligible, no backoff).
 */
class MongoOmdbAttemptStore(db: Option[MongoDatabase], clock: Clock = Clock.systemUTC()) extends OmdbAttemptStore with Logging {
  private val CollectionName = "omdb_attempts"
  private val TtlSeconds     = 90.days.toSeconds

  private val coll: Option[MongoCollection[Document]] =
    db.map(_.getCollection(CollectionName).withWriteConcern(services.tasks.MongoTaskQueue.QueueWriteConcern))

  coll.foreach { c =>
    val t = new Thread(() => ensureTtlIndex(c), s"$CollectionName-init"); t.setDaemon(true); t.start()
  }

  def get(filmKey: String): Option[OmdbAttempt] = coll.flatMap { c =>
    Try(Await.result(c.find(Filters.eq("_id", filmKey)).headOption(), 5.seconds)).toOption.flatten.flatMap { d =>
      Option(d.getDate("at")).map(date => OmdbAttempt(Instant.ofEpochMilli(date.getTime), d.getInteger("level", 0)))
    }
  }

  def record(filmKey: String, level: Int, at: Instant): Unit = coll.foreach { c =>
    Try {
      c.replaceOne(
        Filters.eq("_id", filmKey),
        Document("_id" -> filmKey, "at" -> new java.util.Date(at.toEpochMilli), "level" -> level),
        new ReplaceOptions().upsert(true)
      ).subscribe(
        (_: org.mongodb.scala.result.UpdateResult) => (),
        (exception: Throwable) => logger.debug(s"omdb_attempts write failed for $filmKey: ${exception.getMessage}"))
    }.recover { case exception => logger.debug(s"omdb_attempts write failed for $filmKey: ${exception.getMessage}") }
  }

  private def ensureTtlIndex(c: MongoCollection[Document]): Unit =
    Try(Await.result(c.createIndex(Indexes.ascending("at"), new JIndexOptions().expireAfter(TtlSeconds, TimeUnit.SECONDS)).toFuture(), 10.seconds))
      .recover { case exception => logger.debug(s"$CollectionName TTL index not created: ${exception.getMessage}") }
}
