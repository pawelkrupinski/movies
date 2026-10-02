package services.identity

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

import scala.concurrent.Await
import scala.concurrent.duration._

/** The identity trace over Mongo: a listing's rules and a rule's listings are each one indexed read, a
 *  family's replace drops its old traces, and the whole of it is one bulk write. */
class MongoIdentityTraceStoreIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private def key(n: Int) = ListingKey.Published(s"Venue $n", s"Film $n", None, Nil)
  private def trace(n: Int, family: String, rules: Seq[String]) = ListingTrace(key(n), family, Some(100 + n % 2), "OwnMatch", rules, None)

  "the trace store" should "answer a rule's listings and a film's listings' rules through its indexes, and replace a family's traces" in {
    val client = MongoClient(mongoTarget.uri.value)
    val db     = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "traces"))
    try {
      val store = new MongoIdentityTraceStore(db)
      store.replace(Set.empty, () => Seq(trace(1, "f1", Seq("accept:imdb-suggested", "title:xtra-pokaz-filmu")),
        trace(2, "f1", Seq("join:same-film")), trace(3, "f3", Seq("accept:imdb-suggested"))))
      store.flush()
      val c = db.getCollection[Document](MongoIdentityTraceStore.Collection)
      def ids(filter: org.bson.conversions.Bson) = Await.result(c.find(filter).toFuture(), 30.seconds).map(_.toBsonDocument.getString("_id").getValue).toSet
      // a rule's listings
      ids(Filters.equal("rules", "accept:imdb-suggested")) shouldBe Set(key(1), key(3)).map(ListingKey.serialised)
      // a film's listings, with their rules
      ids(Filters.equal("film", 101)) shouldBe Set(key(1), key(3)).map(ListingKey.serialised)
      // each read uses an index, not a scan
      Seq("rules" -> "accept:imdb-suggested", "film" -> 101, "family" -> "f1").foreach { case (field, value) =>
        val plan = Await.result(c.find(Filters.equal(field, value)).explain[Document]().toFuture(), 30.seconds).toJson()
        withClue(s"$field: ")(plan should include ("IXSCAN"))
      }
      // re-resolving family f1 replaces its traces: listing 2 left it
      store.replace(Set("f1"), () => Seq(trace(1, "f1", Seq("accept:sole-result"))))
      store.flush()
      ids(Filters.equal("family", "f1")) shouldBe Set(ListingKey.serialised(key(1)))
      ids(Filters.equal("rules", "accept:imdb-suggested")) shouldBe Set(ListingKey.serialised(key(3)))
    } finally { Await.result(db.drop().toFuture(), 60.seconds); client.close() }
  }
}
