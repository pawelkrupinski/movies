package services.closure

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** What the closure sweep found a venue to be. */
enum LedgerKind(val stored: String) {
  /** Its sources dropped it and the venue itself shows it has closed: retire it. */
  case Closed extends LedgerKind("closed")
  /** Its sources dropped it, but nothing shows the venue closed: a coverage gap that
   *  needs a new source, never a retirement. */
  case SourceDropped extends LedgerKind("source-dropped")
}

object LedgerKind {
  /** A row written before the split has no kind, and every such row was a closure. */
  def parse(stored: Option[String]): LedgerKind = stored.flatMap(s => values.find(_.stored == s)).getOrElse(Closed)
}

/** One venue's standing verdict: which, since when, and the evidence line it was raised on. */
final case class LedgerEntry(kind: LedgerKind, at: Instant, evidence: String)

/** The venues the closure sweep has raised — read by the web's /uptime page. */
trait ClosureLedgerReader {
  def entries(): Map[String, LedgerEntry]
}

/**
 * The venues the closure sweep has raised, and as what: so each closure pages and asks
 * for its retirement ONCE, each source-dropped venue pages once as a coverage gap, and
 * one that shows life again is withdrawn. Pure storage — the sweep makes every decision.
 */
trait ClosureLedger extends ClosureLedgerReader {
  def record(cinema: String, entry: LedgerEntry): Unit
  def withdraw(cinema: String): Unit
}

final class InMemoryClosureLedger extends ClosureLedger {
  private val byCinema = new ConcurrentHashMap[String, LedgerEntry]()
  def entries(): Map[String, LedgerEntry]               = byCinema.asScala.toMap
  def record(cinema: String, entry: LedgerEntry): Unit  = { byCinema.put(cinema, entry); () }
  def withdraw(cinema: String): Unit                    = { byCinema.remove(cinema); () }
}

/** The `venue_closures` collection in the country's database: `{_id: displayName,
 *  confirmedAt, kind, evidence}`. A handful of rows, so every read is a whole-collection find. */
final class MongoClosureLedger(db: MongoDatabase) extends ClosureLedger {
  private val coll: MongoCollection[Document] = db.getCollection[Document](MongoClosureLedger.Collection)

  def entries(): Map[String, LedgerEntry] =
    Await.result(coll.find().batchSize(tools.MongoReplies.Default).toFuture(), 10.seconds).map(MongoClosureLedger.fromDocument).toMap

  def record(cinema: String, entry: LedgerEntry): Unit = {
    Await.result(coll.replaceOne(Filters.eq("_id", cinema), MongoClosureLedger.toDocument(cinema, entry), ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
    ()
  }

  def withdraw(cinema: String): Unit = {
    Await.result(coll.deleteOne(Filters.eq("_id", cinema)).toFuture(), 10.seconds)
    ()
  }
}

object MongoClosureLedger {
  val Collection = "venue_closures"

  private[closure] def toDocument(cinema: String, entry: LedgerEntry): Document =
    Document("_id" -> cinema, "confirmedAt" -> new java.util.Date(entry.at.toEpochMilli), "kind" -> entry.kind.stored, "evidence" -> entry.evidence)

  private[closure] def fromDocument(d: Document): (String, LedgerEntry) =
    d.getString("_id") -> LedgerEntry(LedgerKind.parse(Option(d.getString("kind"))),
      Instant.ofEpochMilli(d.getDate("confirmedAt").getTime), Option(d.getString("evidence")).getOrElse(""))
}
