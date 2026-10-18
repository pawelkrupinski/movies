package services.closure

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.{MongoDatabase, documentToUntypedDocument}
import services.keyed.{DocCodec, InMemoryKeyedDocStore, KeyedDocStore, MongoKeyedDocStore}

import java.time.Instant

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

/** The ledger over its rows: Mongo and in-memory differ only in the rows. A read or write that
 *  fails throws — the sweep must not take an unread ledger for an empty one. */
class KeyedClosureLedger(rows: KeyedDocStore[LedgerEntry]) extends ClosureLedger {
  def entries(): Map[String, LedgerEntry]              = rows.all()
  def record(cinema: String, entry: LedgerEntry): Unit = rows.put(cinema, entry)
  def withdraw(cinema: String): Unit                   = rows.delete(cinema)
}

object KeyedClosureLedger {
  /** `{_id: displayName, confirmedAt, kind, evidence}`. */
  val Codec: DocCodec[LedgerEntry] = new DocCodec[LedgerEntry] {
    def encode(entry: LedgerEntry): Document =
      Document("confirmedAt" -> java.util.Date.from(entry.at), "kind" -> entry.kind.stored, "evidence" -> entry.evidence)
    def decode(d: Document): Option[LedgerEntry] =
      d.get("confirmedAt").filter(_.isDateTime).map(at => LedgerEntry(LedgerKind.parse(Option(d.getString("kind"))),
        Instant.ofEpochMilli(at.asDateTime.getValue), Option(d.getString("evidence")).getOrElse("")))
  }
}

final class InMemoryClosureLedger extends KeyedClosureLedger(new InMemoryKeyedDocStore(KeyedClosureLedger.Codec))

/** The `venue_closures` collection in the country's database. A handful of rows. */
final class MongoClosureLedger(db: MongoDatabase)
  extends KeyedClosureLedger(new MongoKeyedDocStore(db.getCollection(MongoClosureLedger.Collection), KeyedClosureLedger.Codec))

object MongoClosureLedger {
  val Collection = "venue_closures"
}
