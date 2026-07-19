package services.attempts

import com.mongodb.client.model.UpdateOptions
import org.mongodb.scala.model.{Filters, Sorts, Updates}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, documentToUntypedDocument}
import play.api.Logging
import services.movies.KeysetScan

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * The last enrichment attempt per (rating source, film), keyed by the SAME dedup
 * string the freshness store, the queue and the cadence use (`<site>|tmdb:<id>`)
 * so the /debug expand section can line all four up against one row.
 *
 * Write-only from the worker ([[services.enrichment.CacheRefresher]] records every
 * per-row refresh); read by the web app's debug page through
 * [[EnrichmentAttemptReader]]. Mirrors [[services.cadence.RatingCadenceStore]]'s
 * shape deliberately — same key, same collection conventions, same
 * write-failures-are-debug-logged policy — because the two are read together.
 */
trait EnrichmentAttemptStore {
  /** Record the outcome of one per-row fetch. Never throws: an unreachable Mongo
   *  must not fail the enrichment it is merely observing. */
  def record(key: String, attempt: EnrichmentAttempt): Unit

  def close(): Unit = ()
}

object EnrichmentAttemptStore {
  /** Drops every attempt — the default for scripts/tests and Mongo-less dev, and
   *  what keeps recording strictly optional at every call site. */
  val NoOp: EnrichmentAttemptStore = (_, _) => ()
}

/** In-memory store for tests and Mongo-less local dev. */
class InMemoryEnrichmentAttemptStore extends EnrichmentAttemptStore with EnrichmentAttemptReader {
  private val attempts = new ConcurrentHashMap[String, EnrichmentAttempt]()
  override def record(key: String, attempt: EnrichmentAttempt): Unit = { attempts.put(key, attempt); () }
  override def all(): Seq[(String, EnrichmentAttempt)] = {
    import scala.jdk.CollectionConverters._
    attempts.asScala.toSeq
  }
  override def forKeys(keys: Seq[String]): Map[String, EnrichmentAttempt] =
    keys.flatMap(k => Option(attempts.get(k)).map(k -> _)).toMap
}

/**
 * Mongo-backed store, collection `enrichment_attempts` with documents
 * `{ _id: <key>, at: ISODate, durationMs: Long, outcome: "changed"|"unchanged"|"failed",
 *    detail: String }`.
 *
 * One document per (source, film), overwritten each attempt — see
 * [[EnrichmentAttempt]] on why only the last is kept.
 */
class MongoEnrichmentAttemptStore(db: Option[MongoDatabase]) extends EnrichmentAttemptStore with Logging {
  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection(EnrichmentAttempts.Collection))

  override def record(key: String, attempt: EnrichmentAttempt): Unit = coll.foreach { c =>
    val (outcome, detail) = EnrichmentAttempts.encodeOutcome(attempt.outcome)
    Try {
      c.updateOne(
        Filters.eq("_id", key),
        Updates.combine(
          Updates.set("at",         attempt.at),
          Updates.set("durationMs", attempt.durationMs),
          Updates.set("outcome",    outcome),
          Updates.set("detail",     detail)
        ),
        new UpdateOptions().upsert(true)
      ).subscribe(
        (_: org.mongodb.scala.result.UpdateResult) => (),
        (exception: Throwable) => logger.debug(s"Enrichment-attempt write failed for $key: ${exception.getMessage}")
      )
    }.recover { case exception => logger.debug(s"Enrichment-attempt write failed for $key: ${exception.getMessage}") }
  }
}

/** Read-only view of `enrichment_attempts` for the dev debug page (the web app).
 *  Keyset-paged rather than one unbounded `find()`: this collection is one row per
 *  (source, film) and grows with the corpus, the same shape that made an
 *  unbounded cursor a StackOverflow risk for the cadence hydrate. An
 *  unreadable/absent Mongo yields an empty list rather than throwing. */
trait EnrichmentAttemptReader {
  def all(): Seq[(String, EnrichmentAttempt)]

  /** The attempts for a KNOWN set of keys — what one film's /debug expand needs
   *  (its four `<site>|tmdb:<id>` keys). A bounded `_id in [...]` lookup, so
   *  expanding a row doesn't drag the whole collection across the wire. */
  def forKeys(keys: Seq[String]): Map[String, EnrichmentAttempt]
}

object EnrichmentAttemptReader {
  /** No data — the default where attempts aren't wired (tests, Mongo-less dev). */
  val empty: EnrichmentAttemptReader = new EnrichmentAttemptReader {
    override def all(): Seq[(String, EnrichmentAttempt)] = Seq.empty
    override def forKeys(keys: Seq[String]): Map[String, EnrichmentAttempt] = Map.empty
  }
}

class MongoEnrichmentAttemptReader(db: Option[MongoDatabase]) extends EnrichmentAttemptReader with Logging {
  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection(EnrichmentAttempts.Collection))

  override def all(): Seq[(String, EnrichmentAttempt)] = coll match {
    case None => Seq.empty
    case Some(c) =>
      val collected = Seq.newBuilder[(String, EnrichmentAttempt)]
      Try {
        KeysetScan.scan[Document](
          label          = "EnrichmentAttempt read",
          batchSize      = 2000,
          maxAttempts    = 3,
          initialBackoff = 500.millis,
          keyOf          = _.getString("_id"),
          fetchPage      = (afterId, limit) => {
            val find = afterId.fold(c.find())(a => c.find(Filters.gt("_id", a)))
            Await.result(find.sort(Sorts.ascending("_id")).limit(limit).toFuture(), 30.seconds)
          },
          onIncomplete   = exception => logger.warn(s"Enrichment-attempt read keyset scan failed: ${exception.getMessage}")
        )(batch => batch.foreach(d => EnrichmentAttempts.decodeRecord(d).foreach(collected += _)))
      }.recover { case e => logger.warn(s"Enrichment-attempt read failed: ${e.getMessage}") }
      collected.result()
  }

  override def forKeys(keys: Seq[String]): Map[String, EnrichmentAttempt] =
    if (keys.isEmpty) Map.empty
    else coll.fold(Map.empty[String, EnrichmentAttempt]) { c =>
      Try(Await.result(c.find(Filters.in("_id", keys*)).toFuture(), 10.seconds))
        .map(_.flatMap(EnrichmentAttempts.decodeRecord).toMap)
        .recover { case e => logger.warn(s"Enrichment-attempt lookup failed: ${e.getMessage}"); Map.empty }
        .getOrElse(Map.empty)
    }
}

/** Shared encode/decode so the writer and reader can't drift on the wire format. */
object EnrichmentAttempts {
  val Collection = "enrichment_attempts"

  private val Changed   = "changed"
  private val Unchanged = "unchanged"
  private val Failed    = "failed"

  /** `(outcome tag, detail)` — the detail carries the new badge text for a change
   *  and the error message for a failure; empty for an unchanged fetch. */
  def encodeOutcome(outcome: AttemptOutcome): (String, String) = outcome match {
    case AttemptOutcome.Changed(value)  => (Changed, value)
    case AttemptOutcome.Unchanged       => (Unchanged, "")
    case AttemptOutcome.Failed(message) => (Failed, message)
  }

  def decodeOutcome(tag: String, detail: String): AttemptOutcome = tag match {
    case Changed => AttemptOutcome.Changed(detail)
    case Failed  => AttemptOutcome.Failed(detail)
    case _       => AttemptOutcome.Unchanged
  }

  /** None for a malformed/partial document, so one bad row can't fail the page. */
  def decodeRecord(document: Document): Option[(String, EnrichmentAttempt)] =
    Try {
      val key        = document.getString("_id")
      val at         = document.getDate("at").toInstant
      val durationMs = document.get("durationMs").map(_.asNumber().longValue()).getOrElse(0L)
      val outcome    = decodeOutcome(
        Option(document.getString("outcome")).getOrElse(Unchanged),
        Option(document.getString("detail")).getOrElse("")
      )
      key -> EnrichmentAttempt(at, durationMs, outcome)
    }.toOption
}
