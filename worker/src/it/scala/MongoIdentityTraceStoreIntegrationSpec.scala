package services.identity

import tools.SpecTimeouts

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey

import scala.concurrent.Await

/** The identity trace over Mongo: a listing's rules and a rule's listings are each one indexed read, a
 *  family's replace drops its old traces, and the whole of it is one bulk write. */
class MongoIdentityTraceStoreIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private def key(n: Int) = ListingKey.Published(s"Venue $n", s"Film $n", None, Nil)
  private def trace(n: Int, family: String, rules: Seq[String]) =
    ListingTrace(key(n), family, Some(100 + n % 2), "OwnMatch", rules, None, Seq("director=same_person +4.22", "title=exact +1.50"), Some(100 + n % 2))

  private val Refused = DecisionTrace.Refusal("favoured-calibrated", "below the rating cut", Some(49258), "26.6% < 40.0%")

  "the trace store" should "answer a rule's listings and a film's listings' rules through its indexes, and replace a family's traces" in {
    val client = MongoClient(mongoTarget.uri.value)
    val db     = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "traces"))
    try {
      val store = new MongoIdentityTraceStore(db)
      store.replace(Set.empty, FamilyTraces.of(Seq(trace(1, "f1", Seq("accept:imdb-suggested", "title:xtra-pokaz-filmu")),
        trace(2, "f1", Seq("join:same-film")), trace(3, "f3", Seq("accept:imdb-suggested")),
        trace(4, "f4", Seq(Refused.ruleId)).copy(film = None, refusals = Seq(Refused), blocker = Some("search:found-nothing"),
          searched = Seq("title \"Film 4\": 0 film(s)")),
        trace(5, "f4", Nil).copy(film = None, blocker = Some("search:found-nothing")),
        trace(6, "f4", Nil).copy(film = None, blocker = Some("veto:x"), candidates = Seq("9 2.4% rank 1 DENIED (x) 'Nine'")))))
      store.flush()
      val c = db.getCollection[Document](MongoIdentityTraceStore.Collection)
      def ids(filter: org.bson.conversions.Bson) = Await.result(c.find(filter).toFuture(), SpecTimeouts.Io).map(_.toBsonDocument.getString("_id").getValue).toSet
      // why, with its weights, stored beside the rules
      Await.result(c.find(Filters.equal("_id", ListingKey.serialised(key(1)))).head(), SpecTimeouts.Io).toBsonDocument
        .getArray("evidence").getValues.toString should include ("director=same_person +4.22")
      // a rule's listings
      ids(Filters.equal("rules", "accept:imdb-suggested")) shouldBe Set(key(1), key(3)).map(ListingKey.serialised)
      // a film's listings, with their rules
      ids(Filters.equal("film", 101)) shouldBe Set(key(1), key(3)).map(ListingKey.serialised)
      // each read uses an index, not a scan
      Seq("rules" -> "accept:imdb-suggested", "film" -> 101, "family" -> "f1").foreach { case (field, value) =>
        val plan = Await.result(c.find(Filters.equal(field, value)).explain[Document]().toFuture(), SpecTimeouts.Io).toJson()
        withClue(s"$field: ")(plan should include ("IXSCAN"))
      }
      // each trace reads back whole: its evidence, and why a listing no rule took was refused, by each rule
      def stored(n: Int) = MongoIdentityTraceStore.decode(Await.result(c.find(Filters.equal("_id", ListingKey.serialised(key(n)))).head(), SpecTimeouts.Io).toBsonDocument)
      stored(1).evidence shouldBe Seq("director=same_person +4.22", "title=exact +1.50")
      stored(4).refusals shouldBe Seq(Refused)
      // an unresolved listing keeps what it searched, the candidates it weighed and its blocker, through the sparse blocker index
      stored(4).searched shouldBe Seq("title \"Film 4\": 0 film(s)")
      stored(6).candidates shouldBe Seq("9 2.4% rank 1 DENIED (x) 'Nine'")
      ids(Filters.equal("blocker", "veto:x")) shouldBe Set(ListingKey.serialised(key(6)))
      withClue("blocker: ")(Await.result(c.find(Filters.equal("blocker", "veto:x")).explain[Document]().toFuture(), SpecTimeouts.Io).toJson() should include ("IXSCAN"))
      // a resolved listing carries no blocker field at all, so the sparse index holds only the unresolved
      Await.result(c.countDocuments(Filters.exists("blocker")).toFuture(), SpecTimeouts.Io) shouldBe 3L
      // re-resolving family f1 replaces its traces: listing 2 left it
      store.replace(Set("f1"), FamilyTraces.of(Seq(trace(1, "f1", Seq("accept:sole-result")))))
      store.flush()
      ids(Filters.equal("family", "f1")) shouldBe Set(ListingKey.serialised(key(1)))
      ids(Filters.equal("rules", "accept:imdb-suggested")) shouldBe Set(ListingKey.serialised(key(3)))
    } finally { Await.result(db.drop().toFuture(), SpecTimeouts.Io); client.close() }
  }

  it should "write a restore's traces a batch at a time, never holding them all" in {
    val client = MongoClient(mongoTarget.uri.value)
    val db     = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "traces-batches"))
    try {
      val store = new MongoIdentityTraceStore(db)
      val c     = db.getCollection[Document](MongoIdentityTraceStore.Collection)
      // how many traces were already stored when the 1001st was built — built lazily, as a restore's hand-over is
      @volatile var storedWhenBuilt = -1L
      val traces = (0 until 2500).map(n => FamilyTraces(s"f$n", () => {
        if (n == MongoIdentityTraceStore.WriteBatch) storedWhenBuilt = Await.result(c.countDocuments().toFuture(), SpecTimeouts.Io)
        Seq(trace(n, s"f$n", Seq("accept:sole-result")))
      }))
      store.replace(Set.empty, traces)
      store.flush()
      Await.result(c.countDocuments().toFuture(), SpecTimeouts.Io) shouldBe 2500L
      storedWhenBuilt shouldBe MongoIdentityTraceStore.WriteBatch.toLong
    } finally { Await.result(db.drop().toFuture(), SpecTimeouts.Io); client.close() }
  }

  // Every rules change re-resolves every family at the next boot — and the rules are a digest of all of common —
  // which rewrote all ~165k traces each time as a delete and an upsert, 500-1,100 Mongo writes a second for minutes.
  it should "write only the traces that moved when a family is re-resolved, and delete only those no trace names any more" in {
    val client = MongoClient(mongoTarget.uri.value)
    val db     = client.getDatabase(tools.IntegrationCorpusDatabase.named(mongoTarget, "traces-unchanged"))
    try {
      val store = new MongoIdentityTraceStore(db)
      val c     = db.getCollection[Document](MongoIdentityTraceStore.Collection)
      val first = (0 until 2500).map(n => trace(n, s"f${n / 10}", Seq("accept:sole-result")))
      store.replace(Set.empty, FamilyTraces.of(first))
      store.flush()
      store.written shouldBe 2500L
      // The same families re-resolved to the same decisions: nothing to write.
      val families = first.map(_.family).toSet
      store.replace(families, FamilyTraces.of(first))
      store.flush()
      store.written shouldBe 2500L
      // One trace's rules moved and one listing left its family: one replace, one delete.
      val moved = first.updated(7, trace(7, "f0", Seq("accept:exact-top-hit"))).filterNot(_.listing == key(8))
      store.replace(families, FamilyTraces.of(moved))
      store.flush()
      store.written shouldBe 2502L
      Await.result(c.countDocuments().toFuture(), SpecTimeouts.Io) shouldBe 2499L
      Await.result(c.find(Filters.equal("_id", ListingKey.serialised(key(7)))).toFuture(), SpecTimeouts.Io).head
        .get("rules").get.asArray.getValues.toString should include ("accept:exact-top-hit")
    } finally { Await.result(db.drop().toFuture(), SpecTimeouts.Io); client.close() }
  }
}
