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
 *  index that keeps a month. */
class IdentityHistoryIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {
  private def doc(v: String) = new BsonDocument("ids", BsonString(v))
  private val entries = Seq(
    HistoryEntry("answer:tmdb_queries", "q", 3000L, Some(doc("1")), Some(doc("2")), "b1"),
    HistoryEntry("answer:tmdb_queries", "q", 1000L, None, Some(doc("1")), "b1"),
    HistoryEntry(HistoryEntry.Failure, "https://api.themoviedb.org/3/movie/2", 2000L, None, None, "b1", Some("http 429")),
    HistoryEntry(HistoryEntry.Take, "N|Kino|page|Lalka", 4000L, None, Some(new BsonDocument("tmdb_after", BsonString("2"))), "b2"))

  "the Mongo history" should "read back what it kept, as the in-memory one reads it" in {
    IsolatedMongoDatabase.withDatabase(mongoTarget, "identity-history") { db =>
      val mongo  = new MongoIdentityHistory(db, new services.TtlIndexMismatches)
      val memory = new InMemoryIdentityHistory
      Seq(mongo, memory).foreach(_.append(entries))
      mongo.of("answer:tmdb_queries", Seq("q")) shouldBe memory.of("answer:tmdb_queries", Seq("q"))
      mongo.of("answer:tmdb_queries", Seq("q")).map(_.at) shouldBe Seq(1000L, 3000L)
      mongo.since(Seq(HistoryEntry.Failure, HistoryEntry.Take), 1500L) shouldBe memory.since(Seq(HistoryEntry.Failure, HistoryEntry.Take), 1500L)
      mongo.since(Seq("answer:tmdb_queries"), 1000L).map(_.at) shouldBe Seq(3000L)
      TakeExplanation.explain(mongo, "N|Kino|page|Lalka", 5000L).map(_.take.build) shouldBe Some("b2")
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
