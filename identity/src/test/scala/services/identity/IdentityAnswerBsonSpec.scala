package services.identity

import org.bson.{BsonDocument, BsonInt32, BsonNull, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.measures.Film
import services.identity.vocabulary.FilmCast

/** A film's record is written in ONE format, whichever store keeps it, read back as it was by ONE reader, which never
 *  throws on a field absent, null or unparsable. */
class IdentityAnswerBsonSpec extends AnyFlatSpec with Matchers {
  import IdentityAnswerBsonSpec._

  "a film's record" should "read back with every fact, as the driver writes it" in {
    val written = new org.bson.RawBsonDocument(IdentityAnswerBson.film(Some(roots)).asDocument, new org.bson.codecs.BsonDocumentCodec())
    IdentityAnswerBson.filmOf(written.toBsonDocument) shouldBe Some(roots)
    IdentityAnswerBson.filmOf(IdentityAnswerBson.film(Some(bare))) shouldBe Some(bare)
    IdentityAnswerBson.filmOf(IdentityAnswerBson.film(None)) shouldBe None
  }

  it should "be written in the one format: the cast under `castPairs`, the alternative titles even when none" in {
    IdentityAnswerBson.film(Some(roots)).asDocument.containsKey("castPairs") shouldBe true
    IdentityAnswerBson.film(Some(bare)).asDocument.getArray("alternativeTitles").isEmpty shouldBe true
  }

  // The agreed films' earlier format (FilmBson) kept the cast under `cast`; every one was rewritten into this format
  // (AgreedFilmShapeMigration, 2026-11-21: 39 on the mirror, then 0), and the reader of both shapes went with it
  "a record" should "read its cast from `castPairs` alone, the earlier format's `cast` no more" in {
    IdentityAnswerBson.filmOf(castUnderEarlierName) shouldBe Some(roots.copy(cast = None))
  }

  // the reader once read `alternativeTitles` unguarded, and threw a NullPointerException on a record without it
  it should "read a record without alternative titles as having none" in {
    val noTitles = new BsonDocument("title", BsonString("Klondike")).append("year", BsonInt32(2022))
    IdentityAnswerBson.filmOf(noTitles) shouldBe Some(Film("Klondike", year = Some(2022)))
  }

  it should "read a null field as unknown and a popularity stored as any number, never throw" in {
    val nulls = new BsonDocument("title", BsonString("Klondike")).append("popularity", BsonInt32(3))
      .append("directors", BsonNull()).append("originalTitle", BsonNull()).append("year", BsonNull())
    IdentityAnswerBson.filmOf(nulls) shouldBe Some(Film("Klondike", popularity = Some(3.0)))
  }

  // FilmBson parsed `released` unguarded: one day that does not parse threw out the whole verdict store's read
  it should "read a day that does not parse as none" in {
    IdentityAnswerBson.filmOf(new BsonDocument("title", BsonString("Klondike")).append("released", BsonString("2022-02-30"))) shouldBe
      Some(Film("Klondike"))
  }
}

object IdentityAnswerBsonSpec {
  val roots: Film = Film("Roots", Some("Raíces"), Seq("Raices", "Wurzeln"), Some(1954), Some(83), Some(Seq("Benito Alazraki")),
    Some(Seq("MX")), Some(1.5), 47395, Some(java.time.LocalDate.of(1955, 4, 21)), Some("MXUS"), Seq(85, 80), Some("MX1955P"), 123,
    Some("US=PG"), Some("es"), 18, FilmCast.ofPairs(Seq(Seq(1, 2), Seq(3))))
  val bare: Film = Film("Klondike", year = Some(2022))

  /** [[roots]] in the one format, but its cast under the earlier format's name, `cast`. */
  val castUnderEarlierName: BsonDocument = {
    val d = IdentityAnswerBson.film(Some(roots)).asDocument.clone()
    d.append("cast", d.remove("castPairs"))
  }
}
