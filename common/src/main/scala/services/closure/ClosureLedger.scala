package services.closure

import java.time.Instant

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.{MongoDatabase, documentToUntypedDocument}
import tools.{DocCodec, InMemoryStoreDocuments, KeyedStore, MongoStoreDocuments, StoreReader, StoreWriter}

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

  /** Record `cinema` closed at `at` on `evidence`. */
  final def closed(cinema: String, at: Instant, evidence: String): Unit = record(cinema, LedgerEntry(LedgerKind.Closed, at, evidence))

  /** Record `cinema` dropped by its sources, not shown closed, at `at` on `evidence`. */
  final def sourceDropped(cinema: String, at: Instant, evidence: String): Unit =
    record(cinema, LedgerEntry(LedgerKind.SourceDropped, at, evidence))
}

/** The ledger over its rows: Mongo and in-memory differ only in the rows. A read or write that
 *  fails throws — the sweep must not take an unread ledger for an empty one. */
class KeyedClosureLedger(rows: StoreReader[String, LedgerEntry] & StoreWriter[String, LedgerEntry]) extends ClosureLedger {
  def entries(): Map[String, LedgerEntry]              = rows.all().toMap
  def record(cinema: String, entry: LedgerEntry): Unit = rows.put(cinema, entry)
  def withdraw(cinema: String): Unit                   = { rows.delete(cinema); () }
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

final class InMemoryClosureLedger extends KeyedClosureLedger(KeyedStore.text(new InMemoryStoreDocuments(), KeyedClosureLedger.Codec))

/** The `venue_closures` collection in the country's database. A handful of rows. */
final class MongoClosureLedger(db: MongoDatabase)
  extends KeyedClosureLedger(KeyedStore.text(MongoStoreDocuments.of(db.getCollection(MongoClosureLedger.Collection)), KeyedClosureLedger.Codec))

object MongoClosureLedger {
  val Collection = "venue_closures"
}
