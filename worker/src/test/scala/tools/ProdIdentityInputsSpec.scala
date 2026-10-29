package tools

import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Pin, PinClaim}
import services.movies.ListingKey

import java.time.Instant

/** A recording keeps prod's pins and filed family answers as prod held them, and the live drift check reads them back
 *  whole: one recorded before them holds none, and says so, rather than reading as "no pins, no answers". */
class ProdIdentityInputsSpec extends AnyFlatSpec with Matchers with ScratchDirectories {
  private val pin = Pin(Seq(ListingKey.Published("Kino Muza", "Lalka", None, Nil), ListingKey.Published("Kino Pod Baranami", "Lalka (2026)", None, Nil)),
    PinClaim.IsFilm(1081010), "pawel", "the 2026 adaptation", Instant.parse("2026-10-20T10:00:00Z"))
  private val never = Pin(Seq(ListingKey.Published("Helios", "Rose", None, Nil)), PinClaim.NeverFilm(1176962), "pawel", "not the 2025 Rose",
    Instant.parse("2026-10-21T10:00:00Z"))
  private val families = Map("imdb|title|Lalka" -> new BsonDocument("films", new BsonString("tt3")), "wiki|title|Rose" -> new BsonDocument())

  "a recording's prod inputs" should "read back as written" in {
    val root = tempDirectory("recording")
    ProdIdentityInputs.write(root, "pl", ProdIdentityInputs.Captured(Seq(pin, never), families)) shouldBe
      Seq(ProdIdentityInputs.pinsPath(root, "pl"), ProdIdentityInputs.familiesPath(root, "pl"))
    ProdIdentityInputs.readFrom(root, "pl") shouldBe Some(ProdIdentityInputs.Captured(Seq(pin, never), families))
  }

  they should "be none for a recording made before them, or for another country" in {
    val root = tempDirectory("recording")
    ProdIdentityInputs.readFrom(root, "pl") shouldBe None
    ProdIdentityInputs.write(root, "pl", ProdIdentityInputs.Captured(Nil, Map.empty))
    ProdIdentityInputs.readFrom(root, "de") shouldBe None
    ProdIdentityInputs.readFrom(root, "pl") shouldBe Some(ProdIdentityInputs.Captured(Nil, Map.empty))
  }
}
