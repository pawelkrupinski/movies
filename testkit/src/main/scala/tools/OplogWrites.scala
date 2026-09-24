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

  private def sinceFilter =
    Filters.and(Filters.gt("ts", since), Filters.or(Filters.regex("ns", ns), Filters.regex("o.applyOps.ns", ns)))

  /** Writes to the database since this counter was created. */
  def count(): Long = Await.result(oplog.countDocuments(sinceFilter).toFuture(), Timeout)

  /** WHICH writes: one line per operation since this counter was created — its collection,
   *  the document it hit and, for an update, the fields it set or unset. A count names no
   *  film, and a recording leg's database is gone by the time anyone reads the failure, so
   *  this is the only record of what a pass that should have written nothing wrote. */
  def describe(limit: Int = 20): String = {
    import scala.jdk.CollectionConverters._
    val entries = Await.result(oplog.find(sinceFilter).sort(Sorts.ascending("$natural")).toFuture(), Timeout)
      .map(_.toBsonDocument)
    val ops = entries.flatMap { e =>
      val applyOps = Option(e.get("o")).filter(_.isDocument).flatMap(o => Option(o.asDocument().get("applyOps")))
      applyOps.map(_.asArray().getValues.asScala.toSeq.map(_.asDocument())).getOrElse(Seq(e))
    }
    val prefix = database + "."
    val lines  = ops.filter(op => OplogWrites.str(op, "ns").exists(n => n.startsWith(prefix) &&
      (collections.isEmpty || collections.contains(n.stripPrefix(prefix))))).map(OplogWrites.line)
    if (lines.isEmpty) "" else
      ((s"oplog — ${lines.size} write(s):" +: lines.take(limit).map("  " + _)) ++
        (if (lines.sizeIs > limit) Seq(s"  … and ${lines.size - limit} more") else Nil)).mkString("\n")
  }

  def close(): Unit = client.close()
}

object OplogWrites {
  import org.bson.{BsonDocument, BsonValue}
  import scala.jdk.CollectionConverters._

  private[tools] def str(d: BsonDocument, name: String): Option[String] =
    Option(d.get(name)).filter(_.isString).map(_.asString().getValue)

  /** `movies u Lalka|2026 [diff:{u,d}]` — the op, its collection, the document key, and
   *  for an update the paths it touched. */
  private[tools] def line(op: BsonDocument): String = {
    val collection = str(op, "ns").map(_.dropWhile(_ != '.').drop(1)).getOrElse("?")
    val kind       = str(op, "op").getOrElse("?")
    def idIn(name: String) = Option(op.get(name)).filter(_.isDocument).flatMap(d => Option(d.asDocument().get("_id")))
    // A film's `_id` is opaque; its stored key ("lalka|2026") is what a reader can look up.
    val key = Option(op.get("o")).filter(_.isDocument).flatMap(o => str(o.asDocument(), "key")).map(k => s" ($k)").getOrElse("")
    val id = idIn("o2").orElse(idIn("o")).map(render).getOrElse("?") + key
    val touched = Option(op.get("o")).filter(v => kind == "u" && v.isDocument).map(_.asDocument()).map(paths(_, 3))
    s"$collection $kind $id" + touched.filter(_.nonEmpty).map(t => s" [$t]").getOrElse("")
  }

  /** The keys of an update document, `depth` levels down — enough to name the slot a
   *  `$v:2` diff (`{diff: {u: {...}, sdata: {...}}}`) or a `$set` touched. */
  private def paths(d: BsonDocument, depth: Int): String =
    d.entrySet().asScala.toSeq.map { e =>
      val v: BsonValue = e.getValue
      if (depth > 1 && v.isDocument && !v.asDocument().isEmpty) s"${e.getKey}{${paths(v.asDocument(), depth - 1)}}"
      else e.getKey
    }.take(10).mkString(",")

  private def render(v: BsonValue): String = if (v.isString) v.asString().getValue else v.toString
}
