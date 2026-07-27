package services.fallback

import org.mongodb.scala.Document
import org.mongodb.scala.bson.{BsonArray, BsonDateTime, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class FallbackStoreSpec extends AnyFlatSpec with Matchers {

  private def state(cinema: String, active: Boolean) = FallbackState(
    cinema = cinema, active = active, fallbackSource = "Filmweb", fallbackRef = Some("7"),
    failingSince = Some(Instant.ofEpochMilli(500)),
    since = Some(Instant.ofEpochMilli(1000)), lastReason = Some("down"),
    consecutiveFailures = 1, lastPrimaryProbeAt = Some(Instant.ofEpochMilli(1200)),
    nextPrimaryProbeAt = Some(Instant.ofEpochMilli(2000)), updatedAt = Instant.ofEpochMilli(3000),
    history = List(FallbackEvent(Instant.ofEpochMilli(1000), FallbackEvent.Enter, "down")),
    alerted = active   // exercise both true (active spell) and false through the codec
  )

  "InMemoryFallbackStore" should "store, read back, overwrite and list states" in {
    val store = new InMemoryFallbackStore
    store.get("Kino Praha") shouldBe None
    store.findAll() shouldBe empty

    store.put(state("Kino Praha", active = true))
    store.get("Kino Praha").map(_.active) shouldBe Some(true)

    // put on the same cinema overwrites (keyed by cinema name)
    store.put(state("Kino Praha", active = false))
    store.get("Kino Praha").map(_.active) shouldBe Some(false)

    store.put(state("Kino Elektronik", active = true))
    store.findAll().map(_.cinema).toSet shouldBe Set("Kino Praha", "Kino Elektronik")
  }

  "MongoFallbackStore.fromDocument" should "round-trip a full document incl. string-encoded history" in {
    val document = Document(
      "_id"                 -> "Kino Praha",
      "active"              -> true,
      "filmwebCinemaId"     -> 2180,
      "failingSince"        -> BsonDateTime(500L),
      "since"               -> BsonDateTime(1000L),
      "lastReason"          -> "RuntimeException: down",
      "consecutiveFailures" -> 2,
      "lastPrimaryProbeAt"  -> BsonDateTime(1500L),
      "nextPrimaryProbeAt"  -> BsonDateTime(2000L),
      "updatedAt"           -> BsonDateTime(3000L),
      "history"             -> BsonArray(
        BsonString("1000\tENTER\tdown"),
        BsonString("1500\tPROBE_FAILED\tstill down")
      ),
      "alerted"             -> true
    )

    MongoFallbackStore.fromDocument(document) shouldBe Some(FallbackState(
      cinema = "Kino Praha", active = true, fallbackSource = "Filmweb", fallbackRef = Some("2180"),
      failingSince = Some(Instant.ofEpochMilli(500)),
      since = Some(Instant.ofEpochMilli(1000)), lastReason = Some("RuntimeException: down"),
      consecutiveFailures = 2, lastPrimaryProbeAt = Some(Instant.ofEpochMilli(1500)),
      nextPrimaryProbeAt = Some(Instant.ofEpochMilli(2000)), updatedAt = Instant.ofEpochMilli(3000),
      history = List(
        FallbackEvent(Instant.ofEpochMilli(1000), FallbackEvent.Enter, "down"),
        FallbackEvent(Instant.ofEpochMilli(1500), FallbackEvent.ProbeFailed, "still down")
      ),
      alerted = true
    ))
  }

  it should "default missing optional fields for a minimal (id-only) document" in {
    MongoFallbackStore.fromDocument(Document("_id" -> "Kino X")) shouldBe Some(FallbackState(
      cinema = "Kino X", active = false, fallbackSource = "Filmweb", fallbackRef = None, since = None, lastReason = None,
      consecutiveFailures = 0, lastPrimaryProbeAt = None, nextPrimaryProbeAt = None,
      updatedAt = Instant.EPOCH, history = Nil
    ))
  }

  // Pure write→read round-trip with no Mongo: render the $set toUpdate produces,
  // feed it back through fromDocument. Catches any field-name drift between the two
  // halves of the codec (a typo only a live Mongo would otherwise reveal).
  "MongoFallbackStore write→read" should "round-trip a state through toUpdate + fromDocument" in {
    val s = state("Kino Praha", active = true)
    val set = MongoFallbackStore.toUpdate(s)
      .toBsonDocument(classOf[org.bson.BsonDocument], com.mongodb.MongoClientSettings.getDefaultCodecRegistry)
      .getDocument("$set")
    MongoFallbackStore.fromDocument(Document(set) + ("_id" -> s.cinema)) shouldBe Some(s)
  }
}
