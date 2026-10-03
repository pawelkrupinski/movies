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
      def ids(filter: org.bson.conversions.Bson) = Await.result(c.find(filter).toFuture(), 30.seconds).map(_.toBsonDocument.getString("_id").getValue).toSet
      // why, with its weights, stored beside the rules
      Await.result(c.find(Filters.equal("_id", ListingKey.serialised(key(1)))).head(), 30.seconds).toBsonDocument
        .getArray("evidence").getValues.toString should include ("director=same_person +4.22")
      // a rule's listings
      ids(Filters.equal("rules", "accept:imdb-suggested")) shouldBe Set(key(1), key(3)).map(ListingKey.serialised)
      // a film's listings, with their rules
      ids(Filters.equal("film", 101)) shouldBe Set(key(1), key(3)).map(ListingKey.serialised)
      // each read uses an index, not a scan
      Seq("rules" -> "accept:imdb-suggested", "film" -> 101, "family" -> "f1").foreach { case (field, value) =>
        val plan = Await.result(c.find(Filters.equal(field, value)).explain[Document]().toFuture(), 30.seconds).toJson()
        withClue(s"$field: ")(plan should include ("IXSCAN"))
      }
      // the admin page's reads: a rule's, a film's, a title's listings, and every rule's count
      val reads = new MongoIdentityTraceReads(db)
      reads.byRule("accept:imdb-suggested", 10).map(_.listing).toSet shouldBe Set(key(1), key(3))
      reads.byFilm(101, 10).map(_.listing).toSet shouldBe Set(key(1), key(3))
      reads.byTitle("FILM 2", 10).map(_.listing) shouldBe Seq(key(2))
      reads.byRule("accept:imdb-suggested", 10).find(_.listing == key(1)).map(_.evidence) shouldBe Some(Seq("director=same_person +4.22", "title=exact +1.50"))
      // why a listing no rule took was refused, by each rule: the condition, the candidate weighed, what it said
      reads.byRule(Refused.ruleId, 10).map(_.refusals) shouldBe Seq(Seq(Refused))
      // what keeps listings unresolved, ranked, through the sparse blocker index — and each blocker's listings
      reads.blockers() shouldBe Seq(BlockerCount("search:found-nothing", 2, 2, Seq("Film 4", "Film 5")), BlockerCount("veto:x", 1, 1, Seq("Film 6")))
      reads.byBlocker("veto:x", 10).map(_.candidates) shouldBe Seq(Seq("9 2.4% rank 1 DENIED (x) 'Nine'"))
      reads.byBlocker("search:found-nothing", 10).flatMap(_.searched) shouldBe Seq("title \"Film 4\": 0 film(s)")
      // what a model is asked about: the unresolved listings it wants, read past the ones it does not
      reads.unresolved(10, _ => true).map(_.listing).toSet shouldBe Set(key(4), key(5), key(6))
      reads.unresolved(1, _.listing != key(4)).size shouldBe 1
      reads.unresolved(10, _.listing == key(6)).map(_.listing) shouldBe Seq(key(6))
      withClue("blocker: ")(Await.result(c.find(Filters.equal("blocker", "veto:x")).explain[Document]().toFuture(), 30.seconds).toJson() should include ("IXSCAN"))
      // a resolved listing carries no blocker field at all, so the sparse index holds only the unresolved
      Await.result(c.countDocuments(Filters.exists("blocker")).toFuture(), 30.seconds) shouldBe 3L
      reads.ruleCounts().toMap shouldBe Map("accept:imdb-suggested" -> 2, "title:xtra-pokaz-filmu" -> 1, "join:same-film" -> 1, Refused.ruleId -> 1)
      // re-resolving family f1 replaces its traces: listing 2 left it
      store.replace(Set("f1"), FamilyTraces.of(Seq(trace(1, "f1", Seq("accept:sole-result")))))
      store.flush()
      ids(Filters.equal("family", "f1")) shouldBe Set(ListingKey.serialised(key(1)))
      ids(Filters.equal("rules", "accept:imdb-suggested")) shouldBe Set(ListingKey.serialised(key(3)))
    } finally { Await.result(db.drop().toFuture(), 60.seconds); client.close() }
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
        if (n == MongoIdentityTraceStore.WriteBatch) storedWhenBuilt = Await.result(c.countDocuments().toFuture(), 30.seconds)
        Seq(trace(n, s"f$n", Seq("accept:sole-result")))
      }))
      store.replace(Set.empty, traces)
      store.flush()
      Await.result(c.countDocuments().toFuture(), 30.seconds) shouldBe 2500L
      storedWhenBuilt shouldBe MongoIdentityTraceStore.WriteBatch.toLong
    } finally { Await.result(db.drop().toFuture(), 60.seconds); client.close() }
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
      Await.result(c.countDocuments().toFuture(), 30.seconds) shouldBe 2499L
      Await.result(c.find(Filters.equal("_id", ListingKey.serialised(key(7)))).toFuture(), 30.seconds).head
        .get("rules").get.asArray.getValues.toString should include ("accept:exact-top-hit")
    } finally { Await.result(db.drop().toFuture(), 60.seconds); client.close() }
  }
}
