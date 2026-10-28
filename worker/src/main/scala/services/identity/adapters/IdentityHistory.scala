package services.identity
package adapters

import org.bson.{BsonDateTime, BsonDocument, BsonString}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, Indexes, InsertManyOptions, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging

import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * One moment of the identity system's history ([[IdentityHistory]]): what one input or take was before and after, at
 * `at` (epoch millis), written by `build`.
 *
 *  - `answer:<collection>` — a stored answer whose value moved (or that was deleted), `id` its document's: TMDB's and
 *    IMDb's (`tmdb_films`, `tmdb_queries`, `tmdb_people`), the families' (`identity_family_answers`) and the question
 *    memory's markers of what failed or came to nothing (`identity_question_markers`), each as an answer reads it
 *    ([[TmdbDocuments.project]]): what the model and the agreement could read at any moment;
 *  - `failure` — a read of TMDB or IMDb that failed (a 429, a timeout, a circuit open), `id` its redacted URL, `failure`
 *    what failed: an answer it filed nothing for;
 *  - `listing` — a listing the model took in, re-published otherwise, or let go, `id` its key ([[ListingBson]]);
 *  - `take` — a listing whose film the projection moved, `id` its key: the take before and after, their bases, the
 *    decision's members and the answers it read ([[TakeRecord]]).
 */
final case class HistoryEntry(kind: String, id: String, at: Long, before: Option[BsonDocument], after: Option[BsonDocument],
                              build: String, failure: Option[String] = None,
                              /** `before` not known — a writer that keeps no read of what it replaced (a family answer, a
                               *  marker, a venue page this process had not seen): `None` then says nothing, not "absent". */
                              beforeUnknown: Boolean = false)

object HistoryEntry {
  val Listing = "listing"
  val Take    = "take"
  val Failure = "failure"
  def answer(kind: TmdbKind): String = s"answer:${kind.collection}"
  val Answers: Seq[String] = TmdbKind.values.toSeq.map(answer)
}

/** Where the history is appended: the worker's stores and projection write through it, never read. A write that fails
 *  is logged and dropped — the history explains the system, it never stops it. */
trait IdentityHistory {
  def append(entries: Seq[HistoryEntry]): Unit
}

/** The history read back, for a replay or an explanation ([[IdentityHistoryReplay]], [[TakeExplanation]]). */
trait IdentityHistoryReader {
  /** Every entry of `kinds` written after `from` (epoch millis), oldest first. */
  def since(kinds: Seq[String], from: Long): Seq[HistoryEntry]
  /** The entries of `kind` about `ids`, oldest first. */
  def of(kind: String, ids: Seq[String]): Seq[HistoryEntry]
}

object IdentityHistory {
  /** Nothing kept: a harness, a spec that reads none. */
  val Discard: IdentityHistory = _ => ()
  /** How long an entry is kept: a month of flips can be replayed and explained. */
  val Kept: FiniteDuration = 30.days

  /** The ways a harness's history may differ from the worker's, each with why — chosen where the harness is wired,
   *  never read from configuration. */
  enum Override(val reason: String) {
    /** A convergence or measure leg resolves a recording, not prod: no moment of it is replayed or explained, and its
     *  writes cost the leg's tick (red #2301: Germany's first tick past its 30-minute timeout). */
    case Unwritten extends Override("a harness resolves a recording, not prod: nothing of it is a moment to explain")
  }

  /** The history `writer` keeps — or, a harness `Unwritten` ([[Override]]), none. */
  def composed(writer: => IdentityHistory, overrides: Set[Override]): IdentityHistory =
    if (overrides(Override.Unwritten)) Discard else writer

  /** What `entries` (one id's, any order) say it read at `at`, given what it reads now (`current`): the value the last
   *  entry at or before `at` left, else the value the first entry after `at` replaced, else — no entry moved it since —
   *  what it is now. So only the entries written after `at` are needed. */
  def valueAt(entries: Seq[HistoryEntry], at: Long, current: => Option[BsonDocument]): Option[BsonDocument] = {
    val (until, after) = entries.sortBy(_.at).partition(_.at <= at)
    until.lastOption.map(_.after).orElse(after.headOption.filterNot(_.beforeUnknown).map(_.before)).getOrElse(current)
  }
}

/** The history kept in memory: a spec's, a harness's — the one the Mongo store is held to. */
final class InMemoryIdentityHistory extends IdentityHistory with IdentityHistoryReader {
  private val held = new ConcurrentLinkedQueue[HistoryEntry]()
  // each entry as the Mongo store keeps it: written to BSON and read back
  def append(entries: Seq[HistoryEntry]): Unit =
    entries.foreach(e => held.add(MongoIdentityHistory.entryOf(services.BsonRoundTrip(MongoIdentityHistory.docOf(e)))))
  def all: Seq[HistoryEntry] = held.asScala.toSeq.sortBy(_.at)
  def since(kinds: Seq[String], from: Long): Seq[HistoryEntry] = all.filter(e => kinds.contains(e.kind) && e.at > from)
  def of(kind: String, ids: Seq[String]): Seq[HistoryEntry] = { val wanted = ids.toSet; all.filter(e => e.kind == kind && wanted(e.id)) }
}

/** `identity_history`: one document per entry, expired [[IdentityHistory.Kept]] after it was written (a TTL index on
 *  `at`), read by kind and id, or by kind and time. */
final class MongoIdentityHistory(db: MongoDatabase, ttlMismatches: services.TtlIndexMismatches)
    extends IdentityHistory with IdentityHistoryReader with Logging {
  import MongoIdentityHistory._
  private lazy val coll: MongoCollection[Document] = db.getCollection[Document](Collection)
  /** The collection with its indexes made: the writer's, once — a reader (a tool over the local mirror) makes none. */
  private lazy val indexed: MongoCollection[Document] = {
    services.MongoTtlIndex.reconcile(coll, "at", IdentityHistory.Kept.toSeconds, "MongoIdentityHistory", ttlMismatches)
    Seq(Indexes.ascending("kind", "id", "at"), Indexes.ascending("kind", "at")).foreach { index =>
      scala.util.Try(Await.result(coll.createIndex(index).toFuture(), Timeout))
        .failed.foreach(e => logger.warn(s"identity_history: index ${index.toBsonDocument} not made: $e"))
    }
    coll
  }

  def append(entries: Seq[HistoryEntry]): Unit = if (entries.nonEmpty) {
    try { Await.result(indexed.insertMany(entries.map(e => Document(docOf(e))), InsertManyOptions().ordered(false)).toFuture(), Timeout); () }
    catch { case scala.util.control.NonFatal(e) => logger.warn(s"identity_history: ${entries.size} entr(ies) not kept: $e") }
  }

  def since(kinds: Seq[String], from: Long): Seq[HistoryEntry] =
    Await.result(coll.find(Filters.and(Filters.in("kind", kinds*), Filters.gt("at", BsonDateTime(from)))).sort(Sorts.ascending("at"))
      .batchSize(tools.MongoReplies.Default).toFuture(), ReadTimeout).map(d => entryOf(d.toBsonDocument))

  def of(kind: String, ids: Seq[String]): Seq[HistoryEntry] = if (ids.isEmpty) Nil else
    Await.result(coll.find(Filters.and(Filters.equal("kind", kind), Filters.in("id", ids*))).sort(Sorts.ascending("at"))
      .batchSize(tools.MongoReplies.Default).toFuture(), ReadTimeout).map(d => entryOf(d.toBsonDocument))
}

object MongoIdentityHistory {
  val Collection = "identity_history"
  private val Timeout     = 10.seconds
  private val ReadTimeout = 5.minutes

  def docOf(e: HistoryEntry): BsonDocument = {
    val d = new BsonDocument("kind", BsonString(e.kind)).append("id", BsonString(e.id)).append("at", BsonDateTime(e.at))
      .append("build", BsonString(e.build))
    e.before.foreach(d.append("before", _))
    e.after.foreach(d.append("after", _))
    e.failure.foreach(f => d.append("failure", BsonString(f)))
    if (e.beforeUnknown) d.append("beforeUnknown", org.bson.BsonBoolean.TRUE)
    d
  }

  def entryOf(d: BsonDocument): HistoryEntry =
    HistoryEntry(d.getString("kind").getValue, d.getString("id").getValue, d.getDateTime("at").getValue,
      Option(d.get("before")).filter(_.isDocument).map(_.asDocument), Option(d.get("after")).filter(_.isDocument).map(_.asDocument),
      Option(d.get("build")).filter(_.isString).fold("")(_.asString.getValue), Option(d.get("failure")).filter(_.isString).map(_.asString.getValue),
      Option(d.get("beforeUnknown")).exists(v => v.isBoolean && v.asBoolean.getValue))
}
