package services.closure

import org.mongodb.scala.bson.collection.immutable.Document
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/** The ledger's stored shape: a verdict's kind and evidence survive a write, and a row
 *  written before the closed / source-dropped split reads back as the closure it was. */
class MongoClosureLedgerSpec extends AnyFlatSpec with Matchers {

  private val at = Instant.parse("2026-10-12T09:00:00Z")

  "A ledger row" should "round-trip its kind, time and evidence" in {
    val entry = LedgerEntry(LedgerKind.SourceDropped, at, "Aggregator page gone since 2026-08-31 …")
    MongoClosureLedger.fromDocument(MongoClosureLedger.toDocument("ACME Screening Room Lambertville", entry)) shouldBe
      ("ACME Screening Room Lambertville" -> entry)
  }

  it should "read a row written before the split as a closure" in {
    val legacy = Document("_id" -> "Heimgarten Kino", "confirmedAt" -> new java.util.Date(at.toEpochMilli))
    MongoClosureLedger.fromDocument(legacy) shouldBe ("Heimgarten Kino" -> LedgerEntry(LedgerKind.Closed, at, ""))
  }
}
