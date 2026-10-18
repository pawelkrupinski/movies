package services.attempts

import org.mongodb.scala.{Document, MongoDatabase, documentToUntypedDocument}
import play.api.Logging
import services.keyed.{DocCodec, InMemoryKeyedDocStore, KeyedDocStore, MongoKeyedDocStore}

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
    extends EnrichmentAttemptStore with EnrichmentAttemptReader with Logging {
  override def record(key: String, attempt: EnrichmentAttempt): Unit =
    rows.putDetached(key, attempt)(exception => logger.debug(s"Enrichment-attempt write failed for $key: ${exception.getMessage}"))

  override def retention: services.retention.StampedRows = rows.retention

  /** A failed read THROWS: answered empty, the report said "never attempted" about a film whose
   *  log it simply could not read. */
  override def forKeys(keys: Seq[String]): Map[String, EnrichmentAttempt] =
    if (keys.isEmpty) Map.empty else rows.getAll(keys)

  def all(): Seq[(String, EnrichmentAttempt)] = rows.all().toSeq
}

/** In-memory store for tests and Mongo-less local dev. */
class InMemoryEnrichmentAttemptStore extends KeyedEnrichmentAttemptStore(new InMemoryKeyedDocStore(EnrichmentAttempts.Codec))

/**
 * Mongo-backed store, collection `enrichment_attempts` with documents
 * `{ _id: <key>, at: ISODate, durationMs: Long, outcome: "changed"|"unchanged"|"failed",
 *    detail: String }`. The worker writes it; the web's debug page reads it, from the mirror.
 */
class MongoEnrichmentAttemptStore(db: Option[MongoDatabase]) extends KeyedEnrichmentAttemptStore(
  db.fold(KeyedDocStore.absent[EnrichmentAttempt])(d => new MongoKeyedDocStore(d.getCollection(EnrichmentAttempts.Collection), EnrichmentAttempts.Codec)))

/** Read-only view of `enrichment_attempts` for the dev debug page (the web app): one film's
 *  attempts at a time, never the whole collection. */
trait EnrichmentAttemptReader {
  /** The attempts for a KNOWN set of keys — what one film's /debug expand needs
   *  (its four `<site>|tmdb:<id>` keys). A bounded `_id in [...]` lookup, so
   *  expanding a row doesn't drag the whole collection across the wire. */
  def forKeys(keys: Seq[String]): Map[String, EnrichmentAttempt]
}

object EnrichmentAttemptReader {
  /** No data — the default where attempts aren't wired (tests, Mongo-less dev). */
  val empty: EnrichmentAttemptReader = new EnrichmentAttemptReader {
    override def forKeys(keys: Seq[String]): Map[String, EnrichmentAttempt] = Map.empty
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
