package services

import org.bson.{BsonDateTime, BsonDocument, BsonInt64, BsonSerializationException, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.keyed.{DocCodec, InMemoryKeyedDocStore}
import org.mongodb.scala.bson.collection.immutable.Document

import java.time.Instant

/** An in-memory stand-in for a Mongo store holds what Mongo would, and refuses what Mongo would ([[BsonRoundTrip]]). */
class BsonRoundTripSpec extends AnyFlatSpec with Matchers {

  "A round trip" should "hand a document back as it was written" in {
    val document = new BsonDocument("_id", new BsonString("k")).append("n", new BsonInt64(7)).append("at", new BsonDateTime(1_000))
    BsonRoundTrip(document) shouldBe document
  }

  // 2026-10-25: Mongo refused every agreement verdict whose receipt named a field with a NUL, while every unit spec passed
  it should "refuse a field name with a NUL in it, as the driver's write does" in {
    a[BsonSerializationException] should be thrownBy BsonRoundTrip(new BsonDocument("a\u0000b", new BsonInt64(1)))
  }

  it should "keep only what BSON carries of a value: an instant to the millisecond" in {
    val at = Instant.parse("2026-10-25T10:00:00.123456789Z")
    BsonRoundTrip.via[Instant](i => new BsonDocument("at", new BsonDateTime(i.toEpochMilli)),
      d => Instant.ofEpochMilli(d.getDateTime("at").getValue))(at) shouldBe Instant.parse("2026-10-25T10:00:00.123Z")
  }

  private object Counts extends DocCodec[Map[String, Int]] {
    def encode(value: Map[String, Int]): Document  = Document("counts" -> Document(value.map { case (k, v) => k -> v }))
    def decode(document: Document): Option[Map[String, Int]] =
      document.get("counts").map(_.asDocument).map(d => d.keySet.toArray(Array.empty[String]).map(k => k -> d.getInt32(k).getValue).toMap)
  }

  "The in-memory keyed store" should "refuse a value Mongo cannot store" in {
    val store = new InMemoryKeyedDocStore(Counts)
    store.put("ok", Map("a" -> 1))
    store.get("ok") shouldBe Some(Map("a" -> 1))
    a[BsonSerializationException] should be thrownBy store.put("nul", Map("a\u0000b" -> 1))
  }
}
