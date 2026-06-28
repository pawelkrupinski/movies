package services.cadence

import org.mongodb.scala.Document
import org.mongodb.scala.bson.{BsonDateTime, BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class RatingCadenceStoreSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-06-27T10:00:00Z")

  "InMemoryRatingCadenceStore" should "be empty for an unseen key" in {
    new InMemoryRatingCadenceStore().statsFor("mc|tmdb:1") shouldBe None
  }

  it should "accumulate the streak across no-change refreshes and reset on change" in {
    val store = new InMemoryRatingCadenceStore()
    store.record("mc|tmdb:1", reportedValue = None, t0).unchangedStreak             shouldBe 1
    store.record("mc|tmdb:1", reportedValue = None, t0.plusSeconds(7200)).unchangedStreak shouldBe 2
    store.statsFor("mc|tmdb:1").map(_.unchangedStreak)                          shouldBe Some(2)
    store.record("mc|tmdb:1", reportedValue = Some("85"), t0.plusSeconds(9000)).unchangedStreak shouldBe 0
  }

  it should "key sources independently" in {
    val store = new InMemoryRatingCadenceStore()
    store.record("mc|tmdb:1", reportedValue = None, t0)
    store.record("imdb|tmdb:1", reportedValue = Some("85"), t0)
    store.statsFor("mc|tmdb:1").map(_.unchangedStreak)   shouldBe Some(1)
    store.statsFor("imdb|tmdb:1").map(_.unchangedStreak) shouldBe Some(0)
  }

  "encodeChange / decodeChange" should "round-trip the from → to transition" in {
    val change = RatingChange(t0, "80", "85")
    val doc    = Document("lastChange" -> MongoRatingCadenceStore.encodeChange(Some(change)))
    MongoRatingCadenceStore.decodeChange(doc, "lastChange") shouldBe Some(change)
  }

  it should "decode a legacy `{at, value}` sub-document (pre-`from`) with an empty from" in {
    val legacy = Document("lastChange" -> BsonDocument(
      "at"    -> BsonDateTime(t0.toEpochMilli),
      "value" -> BsonString("7.2")
    ))
    MongoRatingCadenceStore.decodeChange(legacy, "lastChange") shouldBe Some(RatingChange(t0, "", "7.2"))
  }

  it should "yield None for an absent change field" in {
    MongoRatingCadenceStore.decodeChange(Document("_id" -> "mc|tmdb:1"), "prevChange") shouldBe None
  }
}
