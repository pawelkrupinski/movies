package services.enrichment

import org.mongodb.scala.{Document, MongoDatabase}
import play.api.Logging
import services.keyed.{DocCodec, InMemoryKeyedDocStore, KeyedDocStore, MongoKeyedDocStore}

import java.time.Instant
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
  /** Every recorded attempt, loaded in ONE read. The daily [[OmdbBackfill]] sweep
   *  pre-loads this so its per-film backoff check is an in-memory map lookup, not
   *  a blocking Mongo `find` per candidate row — that per-row read, run
   *  corpus-wide every sweep, is what drained the worker's shared-CPU credit to
   *  the floor. */
  def all(): Map[String, OmdbAttempt]
  def record(filmKey: String, level: Int, at: Instant): Unit
}

object OmdbAttemptStore {
  /** No-op store — every film is always eligible (no backoff). Default for tests
   *  and Mongo-less wiring. */
  val noop: OmdbAttemptStore = new OmdbAttemptStore {
    def get(filmKey: String): Option[OmdbAttempt] = None
    def all(): Map[String, OmdbAttempt] = Map.empty
    def record(filmKey: String, level: Int, at: Instant): Unit = ()
  }
}

/** The store over its rows: Mongo and in-memory differ only in the rows. A read that fails answers
 *  as if nothing were recorded, and a write is fire-and-forget: a lost backoff stamp just costs one
 *  extra OMDb probe. */
class KeyedOmdbAttemptStore(rows: KeyedDocStore[OmdbAttempt]) extends OmdbAttemptStore with Logging {
  def get(filmKey: String): Option[OmdbAttempt] = Try(rows.get(filmKey)).toOption.flatten

  /** One read of the whole (TTL-bounded) collection, keyed by filmKey. Used by the sweep to avoid a
   *  per-row `get`. A short read keeps what it read and fails OPEN for the rest (every unread film
   *  eligible) rather than wedging — that costs OMDb calls, not correctness. */
  def all(): Map[String, OmdbAttempt] = {
    val entries = Map.newBuilder[String, OmdbAttempt]
    val outcome = Try(rows.scan(entries ++= _)).fold(tools.ScanOutcome.Incomplete(_), identity)
    if (!outcome.isComplete)
      logger.warn(s"OmdbAttemptStore.all ${outcome.explain} — the sweep will treat unread films as eligible (fail-open)")
    entries.result()
  }

  def record(filmKey: String, level: Int, at: Instant): Unit =
    rows.putDetached(filmKey, OmdbAttempt(at, level))(exception => logger.debug(s"omdb_attempts write failed for $filmKey: ${exception.getMessage}"))
}

object KeyedOmdbAttemptStore {
  /** `{ _id: <filmKey>, at: ISODate, level: Int }`. */
  val Codec: DocCodec[OmdbAttempt] = new DocCodec[OmdbAttempt] {
    def encode(attempt: OmdbAttempt): Document = Document("at" -> java.util.Date.from(attempt.at), "level" -> attempt.level)
    def decode(document: Document): Option[OmdbAttempt] =
      document.get("at").filter(_.isDateTime).map(date =>
        OmdbAttempt(Instant.ofEpochMilli(date.asDateTime.getValue), document.get("level").filter(_.isNumber).fold(0)(_.asNumber.intValue)))
    override def stampField: Option[String] = Some("at")
  }
}

/** In-memory store for tests / Mongo-less local dev. */
class InMemoryOmdbAttemptStore extends KeyedOmdbAttemptStore(new InMemoryKeyedDocStore(KeyedOmdbAttemptStore.Codec))

/**
 * Mongo-backed store: the `omdb_attempts` collection, written with the task queue's relaxed
 * `{w:1,j:false}` write concern. A 90-day TTL on `at` — well beyond the 30-day backoff cap — reaps
 * stamps for films that left the corpus, so the collection can't grow unbounded. No db disables
 * persistence (every film eligible, no backoff).
 */
class MongoOmdbAttemptStore(db: Option[MongoDatabase]) extends KeyedOmdbAttemptStore(
  db.fold(KeyedDocStore.absent[OmdbAttempt])(d => new MongoKeyedDocStore(
    d.getCollection(MongoOmdbAttemptStore.Collection).withWriteConcern(services.tasks.MongoTaskQueue.QueueWriteConcern),
    KeyedOmdbAttemptStore.Codec, expireAfter = Some(90.days))))

object MongoOmdbAttemptStore {
  val Collection = "omdb_attempts"
}
