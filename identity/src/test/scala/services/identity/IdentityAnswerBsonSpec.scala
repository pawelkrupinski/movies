package services.identity

import org.bson.{BsonArray, BsonDocument, BsonDouble, BsonInt32, BsonNull, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.measures.Film
import services.identity.vocabulary.FilmCast

/** A film's record is written in ONE format, whichever store keeps it, and read back as it was; a record an agreed
 *  verdict kept in the format agreed films had before (`cast`, no empty `alternativeTitles`, nulls) reads as it did. */
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

  // The agreed films' earlier format (FilmBson, until 2026-11-21) left out `alternativeTitles` when there were none:
  // the TMDB records' reader read the field unguarded and threw a NullPointerException on it
  "a record in the agreed films' earlier format" should "read as it was written, its cast under `cast`, no titles left out" in {
    IdentityAnswerBson.filmOf(legacyRoots) shouldBe Some(roots)
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

  private def ints(values: Int*)       = new BsonArray(java.util.Arrays.asList(values.map(BsonInt32(_))*))
  private def strings(values: String*) = new BsonArray(java.util.Arrays.asList(values.map(BsonString(_))*))

  /** [[roots]] as the agreed films' earlier format (FilmBson) wrote it. */
  val legacyRoots: BsonDocument = new BsonDocument("title", BsonString("Roots")).append("originalTitle", BsonString("Raíces"))
    .append("alternativeTitles", strings("Raices", "Wurzeln")).append("year", BsonInt32(1954)).append("runtime", BsonInt32(83))
    .append("directors", strings("Benito Alazraki")).append("countries", strings("MX")).append("popularity", BsonDouble(1.5))
    .append("imdbNumber", BsonInt32(47395)).append("released", BsonString("1955-04-21")).append("releaseCountries", BsonString("MXUS"))
    .append("alternativeRuntimes", ints(85, 80)).append("releases", BsonString("MX1955P")).append("taglineKey", BsonInt32(123))
    .append("certificates", BsonString("US=PG")).append("originalLanguage", BsonString("es")).append("genreBits", BsonInt32(18))
    .append("cast", new BsonArray(java.util.Arrays.asList(ints(1, 2), ints(3))))
}
