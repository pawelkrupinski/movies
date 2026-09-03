package services.users

import models.UserState
import org.bson.{BsonDocument, BsonDocumentWriter, BsonDocumentReader}
import org.bson.codecs.{DecoderContext, EncoderContext}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/**
 * A STORED DOCUMENT THAT CARRIES A FIELD THE CASE CLASS NO LONGER HAS MUST STILL DECODE.
 *
 * `UserState` used to hold `selectedMovies` and `favouriteRooms` — the /plan page's picks. The page
 * is gone and nothing reads them, but every user document written before that still carries both,
 * and `UserCodecs` derives its codec straight from the case class with no DTO in between. Whether
 * the macro codec SKIPS an unrecognised field or THROWS on it decides whether removing the fields
 * is a tidy-up or an outage.
 *
 * It is asserted rather than assumed because this repository has shipped the mirror-image mistake
 * to production TWICE: a DTO field that was required, absent from stored documents, and killed the
 * decode of the whole batch it appeared in — silently, because the caller saw a timeout rather than
 * a parse error. That was a MISSING field where one was expected; this is an EXTRA field where none
 * is. They are different directions through the same codec and neither is safe by inspection.
 *
 * Written against the real `UserCodecs.registry`, not a hand-rolled reader, so it fails if the
 * codec strategy ever changes (a DTO appearing, `IgnoreNone` going away, a strict decoder).
 */
class UserStateLegacyFieldsSpec extends AnyFlatSpec with Matchers {

  private val codec = UserCodecs.registry.get(classOf[UserState])

  /** A document as the /plan era wrote it: today's fields plus the two that were retired. */
  private val legacyDocument: BsonDocument = BsonDocument.parse(
    s"""{
       |  "userId": "u-legacy",
       |  "hiddenFilms": ["Dune"],
       |  "disabledCinemas": ["Multikino"],
       |  "updatedAt": {"$$date": "2026-08-01T12:00:00Z"},
       |  "selectedMovies": ["Conclave", "Wicked"],
       |  "favouriteRooms": ["Helios Posnania|3"]
       |}""".stripMargin)

  "a user document written before the plan page was removed" should "still decode" in {
    val decoded = codec.decode(new BsonDocumentReader(legacyDocument), DecoderContext.builder().build())
    decoded.userId          shouldBe "u-legacy"
    decoded.hiddenFilms     shouldBe Set("Dune")
    decoded.disabledCinemas shouldBe Set("Multikino")
  }

  // The fields the retired ones sat beside must be read correctly, not merely without throwing:
  // a codec that skipped to the end on the first unknown key would pass the test above while
  // silently dropping anything after it. `selectedMovies` is deliberately positioned BEFORE
  // `favouriteRooms` and both AFTER the live fields in the fixture, so ordering is exercised.
  it should "not let the retired fields swallow the ones that follow them" in {
    val reordered = BsonDocument.parse(
      s"""{
         |  "userId": "u-order",
         |  "selectedMovies": ["Old Pick"],
         |  "hiddenFilms": ["Kept"],
         |  "favouriteRooms": ["Helios|1"],
         |  "disabledCinemas": ["Kino Muza"],
         |  "updatedAt": {"$$date": "2026-08-01T12:00:00Z"}
         |}""".stripMargin)
    val decoded = codec.decode(new BsonDocumentReader(reordered), DecoderContext.builder().build())
    withClue("a live field written AFTER a retired one must survive the skip: ") {
      decoded.hiddenFilms     shouldBe Set("Kept")
      decoded.disabledCinemas shouldBe Set("Kino Muza")
    }
  }

  // And the write side: what this app stores from now on must no longer carry them, or the
  // "retired" fields would be rewritten on every sync and never actually leave the collection.
  it should "not write the retired fields back out" in {
    val doc    = new BsonDocument()
    val writer = new BsonDocumentWriter(doc)
    codec.encode(writer, UserState("u-new", Set("H"), Set("C"), Instant.parse("2026-09-01T00:00:00Z")),
                 EncoderContext.builder().build())
    doc.keySet.contains("selectedMovies") shouldBe false
    doc.keySet.contains("favouriteRooms") shouldBe false
  }
}
