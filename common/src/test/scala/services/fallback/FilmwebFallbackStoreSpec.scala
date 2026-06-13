package services.fallback

import org.mongodb.scala.Document
import org.mongodb.scala.bson.{BsonArray, BsonDateTime, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class FilmwebFallbackStoreSpec extends AnyFlatSpec with Matchers {

  private def state(cinema: String, active: Boolean) = FilmwebFallbackState(
    cinema = cinema, active = active, filmwebCinemaId = Some(7),
    since = Some(Instant.ofEpochMilli(1000)), lastReason = Some("down"),
    consecutiveFailures = 1, lastPrimaryProbeAt = Some(Instant.ofEpochMilli(1200)),
    nextPrimaryProbeAt = Some(Instant.ofEpochMilli(2000)), updatedAt = Instant.ofEpochMilli(3000),
    history = List(FallbackEvent(Instant.ofEpochMilli(1000), FallbackEvent.Enter, "down")),
    alerted = active   // exercise both true (active spell) and false through the codec
  )

  "InMemoryFilmwebFallbackStore" should "store, read back, overwrite and list states" in {
    val store = new InMemoryFilmwebFallbackStore
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

  "MongoFilmwebFallbackStore.fromDoc" should "round-trip a full document incl. string-encoded history" in {
    val doc = Document(
      "_id"                 -> "Kino Praha",
      "active"              -> true,
      "filmwebCinemaId"     -> 2180,
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

    MongoFilmwebFallbackStore.fromDoc(doc) shouldBe Some(FilmwebFallbackState(
      cinema = "Kino Praha", active = true, filmwebCinemaId = Some(2180),
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
    MongoFilmwebFallbackStore.fromDoc(Document("_id" -> "Kino X")) shouldBe Some(FilmwebFallbackState(
      cinema = "Kino X", active = false, filmwebCinemaId = None, since = None, lastReason = None,
      consecutiveFailures = 0, lastPrimaryProbeAt = None, nextPrimaryProbeAt = None,
      updatedAt = Instant.EPOCH, history = Nil
    ))
  }

  // Pure write→read round-trip with no Mongo: render the $set toUpdate produces,
  // feed it back through fromDoc. Catches any field-name drift between the two
  // halves of the codec (a typo only a live Mongo would otherwise reveal).
  "MongoFilmwebFallbackStore write→read" should "round-trip a state through toUpdate + fromDoc" in {
    val s = state("Kino Praha", active = true)
    val set = MongoFilmwebFallbackStore.toUpdate(s)
      .toBsonDocument(classOf[org.bson.BsonDocument], com.mongodb.MongoClientSettings.getDefaultCodecRegistry)
      .getDocument("$set")
    MongoFilmwebFallbackStore.fromDoc(Document(set) + ("_id" -> s.cinema)) shouldBe Some(s)
  }
}
