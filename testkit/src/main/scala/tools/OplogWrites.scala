package tools

import org.bson.BsonTimestamp
import org.mongodb.scala.bson.Document
import org.mongodb.scala.model.{Filters, Sorts}
import org.mongodb.scala.{MongoClient, ObservableFuture, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * How many writes Mongo actually performed against one database since a starting point —
 * counted off the replica set's oplog, which is the only place every write shows up.
 *
 * The change stream is not good enough for a fixpoint assertion against real Mongo: it
 * delivers asynchronously, so "no event arrived" only means "none arrived yet". The
 * repository's own counters are not good enough either: a byte-identical `replaceOne` is
 * reported by the driver as a modification, writes an oplog entry, and is delivered to
 * every change-stream consumer — the `6365b8e95` film-document rewrite was invisible to
 * everything but this count, and its commit measured it exactly this way. Reading the oplog
 * is synchronous and exact: whatever a pass wrote is in it by the time the pass returns.
 *
 * Transactions (the staging fold commits in one) land as a single `applyOps` entry on
 * `admin.$cmd`; each is counted once per transaction whose operations touch the database
 * (or, when `collections` names some, touch one of them).
 */
final class OplogWrites(uri: String, database: String, collections: Seq[String] = Nil) extends AutoCloseable {
  private val client = MongoClient(uri)
  private val oplog  = client.getDatabase("local").getCollection[Document]("oplog.rs")
  private val Timeout = 30.seconds

  // Every collection of the database, or only the named ones — a pass that is allowed its
  // bookkeeping (a task claimed, a freshness stamp) can still be held to zero corpus writes.
  private val ns = "^" + java.util.regex.Pattern.quote(database + ".") +
    (if (collections.isEmpty) "" else collections.map(java.util.regex.Pattern.quote).mkString("(?:", "|", ")$"))

  /** The newest entry at construction; every count is of entries after it. */
  private val since: BsonTimestamp =
    Await.result(oplog.find().sort(Sorts.descending("$natural")).limit(1).toFuture(), Timeout)
      .headOption.flatMap(_.get("ts")).map(_.asTimestamp()).getOrElse(new BsonTimestamp(0, 0))

  /** Writes to the database since this counter was created. */
  def count(): Long =
    Await.result(oplog.countDocuments(Filters.and(
      Filters.gt("ts", since),
      Filters.or(Filters.regex("ns", ns), Filters.regex("o.applyOps.ns", ns)))).toFuture(), Timeout)

  def close(): Unit = client.close()
}
