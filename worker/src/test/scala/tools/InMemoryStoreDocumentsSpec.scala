package tools

import java.time.{Duration, Instant}

import scala.concurrent.duration.*

import org.bson.{BsonDateTime, BsonDocument, BsonInt32, BsonInt64, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the in-memory documents do that a Mongo server does on its own -- expire by a TTL, order `_id`s -- here on the
 *  injected clock ([[KeyedStoreContractSpec]] holds the behaviour both implementations share). */
class InMemoryStoreDocumentsSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-11-26T12:00:00Z")
  private def stamped(id: String, at: Instant) =
    new BsonDocument("_id", new BsonString(id)).append("at", new BsonDateTime(at.toEpochMilli))

  "InMemoryStoreDocuments with an expiry" should "delete a document once the clock reads past its stamp plus the expiry" in {
    val clock     = new MutableClock(t0)
    val documents = InMemoryStoreDocuments.expiring(DocumentExpiry("at", 1.hour), clock)
    documents.replace(Seq(stamped("old", t0.minusSeconds(1800)), stamped("new", t0)))
    documents.replace(Seq(new BsonDocument("_id", new BsonString("unstamped"))))
    clock.advance(Duration.ofMinutes(29))
    documents.find(Seq("old", "new").map(BsonString(_))).map(_.getString("_id").getValue) shouldBe Seq("old", "new")
    clock.advance(Duration.ofMinutes(1))
    documents.find(Seq("old", "new").map(BsonString(_))).map(_.getString("_id").getValue) shouldBe Seq("new")
    clock.advance(Duration.ofHours(10))
    var ids = Seq.empty[String]
    documents.scan(page => ids ++= page.map(_.getString("_id").getValue)) shouldBe ScanOutcome.Complete
    ids shouldBe Seq("unstamped")
  }

  it should "free an expired key for a new insert, as Mongo's monitor does" in {
    val clock     = new MutableClock(t0)
    val documents = InMemoryStoreDocuments.expiring(DocumentExpiry("at", 1.minute), clock)
    documents.insert(stamped("claim", t0)) shouldBe true
    documents.insert(stamped("claim", t0)) shouldBe false
    clock.advance(Duration.ofMinutes(1))
    documents.insert(stamped("claim", clock.instant())) shouldBe true
  }

  "InMemoryStoreDocuments' _id order" should "be Mongo's: numbers by value whatever their type, before strings by UTF-8 bytes" in {
    val ids = Seq(new BsonString("b"), new BsonInt64(10), new BsonString("ﬁ"), new BsonInt32(-3), new BsonString("😀"),
      new BsonString("a"), new BsonInt32(2))
    ids.sortWith((a, b) => InMemoryStoreDocuments.IdOrder.compare(a, b) < 0) shouldBe
      Seq(new BsonInt32(-3), new BsonInt32(2), new BsonInt64(10), new BsonString("a"), new BsonString("b"), new BsonString("ﬁ"),
        new BsonString("😀"))
  }
}
