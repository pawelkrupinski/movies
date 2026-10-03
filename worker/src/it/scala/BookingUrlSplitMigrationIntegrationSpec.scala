package services.movies

import models.{CityScreening, Showtime}
import org.bson.BsonDocument
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.MongoReadModelRepository

import java.time.LocalDateTime
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Rows written before booking URLs were split stay whole until something rewrites them, and
 * nothing does: both collections skip a write whose row decodes unchanged. The migration
 * rewrites each once, never over a newer write, and remembers a finished collection.
 */
class BookingUrlSplitMigrationIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val clock = java.time.Clock.fixed(java.time.Instant.parse("2026-10-03T10:00:00Z"), java.time.ZoneOffset.UTC)
  private val at = """{ "$date": "2026-12-17T13:00:00Z" }"""
  private def showtime(url: String) = s"""{ "dateTime": $at, "bookingUrl": "$url", "format": [] }"""
  private val whole = s"""[${showtime("https://kino.example/buy?show=101")}, ${showtime("https://kino.example/buy?show=2")}]"""

  private def withDatabase(body: MongoDatabase => Unit): Unit = {
    val client = MongoClient(mongoTarget.uri.value)
    val db     = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "booking-url-split"))
    try body(db) finally { Await.result(db.drop().toFuture(), 60.seconds); client.close() }
  }

  private def insert(db: MongoDatabase, collection: String, json: String*): Unit = {
    Await.result(db.getCollection[BsonDocument](collection).insertMany(json.map(BsonDocument.parse)).toFuture(), 30.seconds)
    ()
  }
  private def stored(db: MongoDatabase, collection: String, id: String): BsonDocument =
    Await.result(db.getCollection[BsonDocument](collection).find(Filters.eq("_id", id)).head(), 30.seconds)

  "the migration" should "rewrite every whole-URL row split at its prefix, reading back the same showtimes" in withDatabase { db =>
    val updated = """{ "$date": "2026-09-30T10:00:00Z" }"""
    insert(db, ScreeningsRepository.Collection,
      s"""{ "_id": "a", "filmId": "a", "slotKey": "Helios", "showtimes": $whole, "updatedAt": $updated, "listingKey": null }""",
      s"""{ "_id": "b", "filmId": "b", "slotKey": "Helios", "showtimes": [${showtime("https://x/1")}], "updatedAt": $updated, "listingKey": null }""")
    insert(db, MongoReadModelRepository.ScreeningsCollection,
      s"""{ "_id": "a", "filmId": "a", "city": "poznan", "cinema": "Helios", "showtimes": $whole, "listingKeys": [] }""")

    val outcomes = new BookingUrlSplitMigration(db, clock).run()

    outcomes.map(o => (o.rewritten, o.complete)) shouldBe Seq((1L, true), (1L, true))
    val row = stored(db, ScreeningsRepository.Collection, "a")
    row.getString("bookingUrlPrefix").getValue shouldBe "https://kino.example/buy?show="
    row.toJson should not include(""""bookingUrl":""")
    val urls = Seq(Some("https://kino.example/buy?show=101"), Some("https://kino.example/buy?show=2"))
    MovieCodecs.registry.get(classOf[StoredScreeningsDto]).decode(new org.bson.BsonDocumentReader(row), org.bson.codecs.DecoderContext.builder().build())
      .showtimes.map(_.bookingUrl) shouldBe urls
    val served = stored(db, MongoReadModelRepository.ScreeningsCollection, "a")
    services.readmodel.ReadModelCodecs.registry.get(classOf[CityScreening])
      .decode(new org.bson.BsonDocumentReader(served), org.bson.codecs.DecoderContext.builder().build())
      .showtimes shouldBe urls.map(url => Showtime(LocalDateTime.of(2026, 12, 17, 13, 0), url))
    // A row whose URLs share no prefix is left as it was.
    stored(db, ScreeningsRepository.Collection, "b").containsKey("bookingUrlPrefix") shouldBe false
  }

  it should "not scan a collection again once a pass over it completed" in withDatabase { db =>
    val migration = new BookingUrlSplitMigration(db, clock)
    migration.run()
    insert(db, MongoReadModelRepository.ScreeningsCollection,
      s"""{ "_id": "late", "filmId": "a", "city": "poznan", "cinema": "Helios", "showtimes": $whole, "listingKeys": [] }""")
    migration.run().map(_.rewritten) shouldBe Seq(0L, 0L)
    stored(db, MongoReadModelRepository.ScreeningsCollection, "late").containsKey("bookingUrlPrefix") shouldBe false
  }

  "a row rewritten after the pass read it" should "keep the newer write" in withDatabase { db =>
    insert(db, MongoReadModelRepository.ScreeningsCollection,
      s"""{ "_id": "a", "filmId": "a", "city": "poznan", "cinema": "Helios", "showtimes": $whole, "listingKeys": [] }""")
    val read = stored(db, MongoReadModelRepository.ScreeningsCollection, "a")
    val replacement = new BookingUrlSplitMigration(db, clock)
      .resplit(read, services.readmodel.ReadModelCodecs.registry.get(classOf[CityScreening])).get
    // A projection rewrites the row between the pass's read and its write.
    val newer = BsonDocument.parse(s"""{ "s": [${showtime("https://kino.example/buy?show=7")}] }""").get("s")
    Await.result(db.getCollection[BsonDocument](MongoReadModelRepository.ScreeningsCollection)
      .updateOne(Filters.eq("_id", "a"), org.mongodb.scala.model.Updates.set("showtimes", newer)).toFuture(), 30.seconds)
    Await.result(db.getCollection[BsonDocument](MongoReadModelRepository.ScreeningsCollection)
      .bulkWrite(Seq(replacement)).toFuture(), 30.seconds).getMatchedCount shouldBe 0
    stored(db, MongoReadModelRepository.ScreeningsCollection, "a").toJson should include("show=7")
  }
}
