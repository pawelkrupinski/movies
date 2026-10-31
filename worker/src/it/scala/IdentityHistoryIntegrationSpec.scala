package integration

import org.bson.{BsonDocument, BsonString}
import org.mongodb.scala.ObservableFuture
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.adapters.{HistoryEntry, IdentityHistory, InMemoryIdentityHistory, MongoIdentityHistory, TakeExplanation}
import tools.{IsolatedMongoDatabase, SpecTimeouts}

import scala.concurrent.Await
import scala.jdk.CollectionConverters._

/** `identity_history` through a real Mongo: entries read back as written, by kind and id and by kind after a moment,
 *  oldest first — as the in-memory history reads them, which the replay and explanation specs run on — under a TTL
 *  index that keeps a month.
 *
 *  The entries are dated in 2100, never at epoch-millis like 1000: `at` is stored as a date, and an entry dated 1970
 *  is already a month old, so Mongo's TTL monitor (a pass every 60 s) deleted them between two reads of the same test
 *  (`List() was not equal to List(1000, 3000)` in a land check, 2026-10-31). A fixed future moment, not one read off
 *  the wall clock (NoWallClockInTestsSpec): the monitor never expires an entry dated after its own now. */
class IdentityHistoryIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {
  private def doc(v: String) = new BsonDocument("ids", BsonString(v))
  /** The spec's moment zero, 2100-01-01T00:00Z: later than any now the TTL monitor reads, so it keeps every entry. */
  private val t0 = 4102444800000L
  private val entries = Seq(
    HistoryEntry("answer:tmdb_queries", "q", t0 + 3000L, Some(doc("1")), Some(doc("2")), "b1"),
    HistoryEntry("answer:tmdb_queries", "q", t0 + 1000L, None, Some(doc("1")), "b1"),
    HistoryEntry(HistoryEntry.Failure, "https://api.themoviedb.org/3/movie/2", t0 + 2000L, None, None, "b1", Some("http 429")),
    HistoryEntry(HistoryEntry.Take, "N|Kino|page|Lalka", t0 + 4000L, None, Some(new BsonDocument("tmdb_after", BsonString("2"))), "b2"))

  "the Mongo history" should "read back what it kept, as the in-memory one reads it" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "identity-history") { db =>
      val mongo  = new MongoIdentityHistory(db, new services.TtlIndexMismatches)
      val memory = new InMemoryIdentityHistory
      Seq(mongo, memory).foreach(_.append(entries))
      mongo.of("answer:tmdb_queries", Seq("q")) shouldBe memory.of("answer:tmdb_queries", Seq("q"))
      mongo.of("answer:tmdb_queries", Seq("q")).map(_.at) shouldBe Seq(t0 + 1000L, t0 + 3000L)
      val failuresAndTakes = Seq(HistoryEntry.Failure, HistoryEntry.Take)
      mongo.since(failuresAndTakes, t0 + 1500L) shouldBe memory.since(failuresAndTakes, t0 + 1500L)
      mongo.since(Seq("answer:tmdb_queries"), t0 + 1000L).map(_.at) shouldBe Seq(t0 + 3000L)
      TakeExplanation.explain(mongo, "N|Kino|page|Lalka", t0 + 5000L).map(_.take.build) shouldBe Some("b2")
    }
  }

  it should "expire its entries a month after they were written" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "identity-history-ttl") { db =>
      new MongoIdentityHistory(db, new services.TtlIndexMismatches).append(entries.take(1))
      val indexes = Await.result(db.getCollection(MongoIdentityHistory.Collection).listIndexes().toFuture(), SpecTimeouts.Io)
      val ttl     = indexes.find(_.get("expireAfterSeconds").isDefined).get
      ttl.get("key").get.asDocument.keySet.asScala shouldBe Set("at")
      ttl.get("expireAfterSeconds").get.asNumber.longValue shouldBe IdentityHistory.Kept.toSeconds
    }
  }
}
