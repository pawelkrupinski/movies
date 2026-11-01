package services.identity.staging

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.Instant

/** The staging store: a country's clusters and beat as published, read for every country at once, each country from its
 *  own cursor — so a poll carries the clusters written since, and the keys that say which are gone. */
class IdentityStagingSpec extends AnyFlatSpec with Matchers {
  private val clock = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
  private def store() = new IdentityStaging(new InMemoryStagingDocuments, clock)

  private def cluster(key: String) = StagedCluster(key, Seq(StagedListing("Kino Muza", s"Film $key")), 1,
    StagedTake(None, None, "NoEvidence", 0.0), modelGaps = 1, onTmdb = false,
    Seq(StagedQuestion("FamilySearch", "imdb", s"imdb|title|Film $key", StagedQuestion.NotAttempted)), 1, 0, since = clock.millis())
  private def beat(staging: Int) = StagingBeat(clock.millis(), staging, staging, 0, Map("FamilySearch" -> staging), staging)

  "The staging store" should "hand back what a country published, every country at once, its beat included" in {
    val staging = store()
    staging.publish("pl", Seq(cluster("a"), cluster("b")), Nil, beat(2))
    staging.publish("uk", Seq(cluster("c")), Nil, beat(1))

    val read = staging.read(Map.empty)
    read.map(_.country) shouldBe Seq("pl", "uk")
    read.head.changed.map(_.key).sorted shouldBe Seq("a", "b")
    read.head.keys.sorted shouldBe Seq("a", "b")
    read.head.beat.map(_.staging) shouldBe Some(2)
    read(1).changed shouldBe Seq(cluster("c"))
    staging.ofCountry("uk") shouldBe Seq(cluster("c"))
  }

  it should "carry from each country's cursor only the clusters written since, and the keys still held" in {
    val staging = store()
    staging.publish("pl", Seq(cluster("a"), cluster("b")), Nil, beat(2))
    staging.publish("uk", Seq(cluster("c")), Nil, beat(1))
    val cursors = staging.read(Map.empty).flatMap(c => c.cursor.map(c.country -> _)).toMap

    clock.advanceSeconds(20)
    staging.publish("pl", Seq(cluster("d")), removed = Seq("a"), beat(2))
    val read = staging.read(cursors)
    // what was written since, and once more what was written AT the cursor: a publish's writes straddling a read
    read.head.changed.map(_.key).sorted shouldBe Seq("b", "d")
    read.head.keys.sorted shouldBe Seq("b", "d")
    read.head.cursor shouldBe Some(clock.instant())
    // nothing moved in the UK since its cursor: the cluster written AT it again, never one before it
    read(1).changed.map(_.key) shouldBe Seq("c")
    read(1).cursor shouldBe cursors.get("uk")
  }

  it should "read a take written before its explanation was kept as one without" in {
    val docs = new InMemoryStagingDocuments
    val old  = play.api.libs.json.Json.toJson(cluster("a")).as[play.api.libs.json.JsObject]
    val take = (old \ "take").as[play.api.libs.json.JsObject] - "explanation"
    docs.put(Seq(StagingDoc("pl|cluster|a", "pl", "cluster", clock.instant(), play.api.libs.json.Json.stringify(old + ("take" -> take)))))
    new IdentityStaging(docs, clock).ofCountry("pl").map(_.take.explanation) shouldBe Seq(Nil)
  }
}
