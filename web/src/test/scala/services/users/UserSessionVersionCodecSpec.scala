package services.users

import models.User
import org.bson.{BsonDocument, BsonDocumentReader, BsonDocumentWriter}
import org.bson.codecs.{DecoderContext, EncoderContext}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/**
 * `sessionVersion` (added 2026-09-20 for "sign out everywhere" revocation —
 * see `controllers.SignedInUser`) is a field essentially every `users`
 * document already in production does not carry. Same shape, same risk, as
 * `UserState.hiddenFilmsByCountry` (see `UserStateLegacyFieldsSpec`'s own
 * doc comment on why this direction through a macro-derived codec — a field
 * the CLASS has that a stored document does not — has bitten this repo
 * before): asserts the codec fills the default (`0`) for a document missing
 * it rather than throwing, which would turn every pre-existing session
 * check into an outage rather than a quiet "never revoked yet".
 */
class UserSessionVersionCodecSpec extends AnyFlatSpec with Matchers {

  private val codec = UserCodecs.registry.get(classOf[User])

  private val preFieldDocument: BsonDocument = BsonDocument.parse(
    s"""{
       |  "id": "u-legacy",
       |  "provider": "google",
       |  "providerSub": "sub-1",
       |  "email": "legacy@example.com",
       |  "displayName": "Legacy User",
       |  "avatarUrl": null,
       |  "createdAt": {"$$date": "2026-05-19T12:00:00Z"},
       |  "lastSeenAt": {"$$date": "2026-05-19T12:00:00Z"}
       |}""".stripMargin)

  "a users document written before sessionVersion existed" should "decode with sessionVersion 0, not throw" in {
    val decoded = codec.decode(new BsonDocumentReader(preFieldDocument), DecoderContext.builder().build())
    decoded.id             shouldBe "u-legacy"
    decoded.sessionVersion shouldBe 0
  }

  it should "round-trip a non-zero sessionVersion through encode/decode" in {
    val user = User(
      id = "u-revoked", provider = "google", providerSub = "sub-2",
      email = Some("revoked@example.com"), displayName = None, avatarUrl = None,
      createdAt = Instant.parse("2026-09-01T00:00:00Z"), lastSeenAt = Instant.parse("2026-09-01T00:00:00Z"),
      sessionVersion = 4
    )
    val doc    = new BsonDocument()
    val writer = new BsonDocumentWriter(doc)
    codec.encode(writer, user, EncoderContext.builder().build())

    val decoded = codec.decode(new BsonDocumentReader(doc), DecoderContext.builder().build())
    decoded.sessionVersion shouldBe 4
  }
}
