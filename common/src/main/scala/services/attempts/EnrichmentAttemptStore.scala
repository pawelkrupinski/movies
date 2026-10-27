package services.attempts

import org.mongodb.scala.{Document, MongoDatabase, documentToUntypedDocument}
import play.api.Logging
import services.keyed.{DocCodec, InMemoryKeyedDocStore, KeyedDocStore, MongoKeyedDocStore}

import scala.util.Try

/**
 * The last enrichment attempt per (rating source, film), keyed by the SAME dedup
 * string the freshness store, the queue and the cadence use (`<site>|tmdb:<id>`).
 *
 * Write-only from the worker ([[services.enrichment.CacheRefresher]] records every
 * per-row refresh). Mirrors [[services.cadence.RatingCadenceStore]]'s
 * shape deliberately — same key, same collection conventions, same
 * write-failures-are-debug-logged policy.
 */
trait EnrichmentAttemptStore {
  /** Record the outcome of one per-row fetch. Never throws: an unreachable Mongo
   *  must not fail the enrichment it is merely observing. */
  def record(key: String, attempt: EnrichmentAttempt): Unit

  /** Its rows by when each was last written, for the orphan sweep (`OrphanFilmStateSweep`). */
  def retention: services.retention.StampedRows = services.retention.StampedRows.Unswept

  def close(): Unit = ()
}

object EnrichmentAttemptStore {
  /** Drops every attempt — the default for scripts/tests and Mongo-less dev, and
   *  what keeps recording strictly optional at every call site. */
  val NoOp: EnrichmentAttemptStore = (_, _) => ()
}

/**
 * The store over its rows, one document per (source, film) overwritten each attempt — see
 * [[EnrichmentAttempt]] on why only the last is kept. Mongo and in-memory differ only in the rows.
 */
class KeyedEnrichmentAttemptStore(rows: KeyedDocStore[EnrichmentAttempt])
    extends EnrichmentAttemptStore with Logging {
  override def record(key: String, attempt: EnrichmentAttempt): Unit =
    rows.putDetached(key, attempt)(exception => logger.debug(s"Enrichment-attempt write failed for $key: ${exception.getMessage}"))

  override def retention: services.retention.StampedRows = rows.retention

  def all(): Seq[(String, EnrichmentAttempt)] = rows.all().toSeq
}

/** In-memory store for tests and Mongo-less local dev. */
class InMemoryEnrichmentAttemptStore extends KeyedEnrichmentAttemptStore(new InMemoryKeyedDocStore(EnrichmentAttempts.Codec))

/**
 * Mongo-backed store, collection `enrichment_attempts` with documents
 * `{ _id: <key>, at: ISODate, durationMs: Long, outcome: "changed"|"unchanged"|"failed",
 *    detail: String }`. The worker writes it.
 */
class MongoEnrichmentAttemptStore(db: Option[MongoDatabase]) extends KeyedEnrichmentAttemptStore(
  db.fold(KeyedDocStore.absent[EnrichmentAttempt])(d => new MongoKeyedDocStore(d.getCollection(EnrichmentAttempts.Collection), EnrichmentAttempts.Codec)))

/** Shared encode/decode so the writer and its readers can't drift on the wire format. */
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

  /** The document beside `_id`; None from `decode` for a malformed/partial one, so one bad row
   *  can't fail the page. */
  val Codec: DocCodec[EnrichmentAttempt] = new DocCodec[EnrichmentAttempt] {
    def encode(attempt: EnrichmentAttempt): Document = {
      val (outcome, detail) = encodeOutcome(attempt.outcome)
      // By name: the order the store's earlier `$set` upsert wrote them in, so every row reads alike.
      Document("at" -> java.util.Date.from(attempt.at), "detail" -> detail, "durationMs" -> attempt.durationMs, "outcome" -> outcome)
    }
    def decode(document: Document): Option[EnrichmentAttempt] =
      Try {
        val at         = document.getDate("at").toInstant
        val durationMs = document.get("durationMs").map(_.asNumber().longValue()).getOrElse(0L)
        val outcome    = decodeOutcome(
          Option(document.getString("outcome")).getOrElse(Unchanged),
          Option(document.getString("detail")).getOrElse("")
        )
        EnrichmentAttempt(at, durationMs, outcome)
      }.toOption
    override def stampField: Option[String] = Some("at")
  }
}
