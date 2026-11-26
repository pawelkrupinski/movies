package services.identity
package adapters

import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonNull, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.measures.Film
import services.identity.vocabulary.{Hit, PopularityBucket}
import services.tmdb.TmdbStore

/** [[TmdbStore.filmHit]]: a film's hit, which every film a search names is asked for -- off its record when it has one.
 *  It reads the record's four hit fields as the frozen reading below did, which decoded the whole record for them:
 *  most of what a take-up's hits allocated (docs/design/corpus-context-persistence.md). */
class FilmHitSpec extends AnyFlatSpec with Matchers {

  /** The reading before a hit read only its fields, frozen. */
  private def frozen(id: Int, film: BsonDocument): Option[Hit] =
    Option(film.get("record")).filter(_.isDocument).flatMap(IdentityAnswerBson.filmOf)
      .map(f => Hit(id, f.title, f.originalTitle, f.year, f.popularity.getOrElse(PopularityBucket.rounded(0.0))))
      .orElse(Option(film.get("hit")).map(h => TmdbStore.hitOf(id, h.asDocument)))

  private def recorded(film: Film) = new BsonDocument("record", IdentityAnswerBson.film(Some(film)))
  private val rich = Film("Once Upon a Time in America", Some("C'era una volta in America"),
    (1 to 40).map(n => s"Alternative title $n"), Some(1984), Some(229), Some(Seq("Sergio Leone")), Some(Seq("IT", "US")),
    Some(41.7), 87843, Some(java.time.LocalDate.of(1984, 5, 23)), Some("ITUSDEPL"), Seq(139, 251), Some("1984-05-23 ITUSDE"),
    17, Some("GB=18,US=R"), Some("en"), 5)
  private val searchHit = new BsonDocument("title", new BsonString("Lalka")).append("year", new BsonInt32(2025))
    .append("popularity", new BsonInt32(3))

  private val films: Seq[BsonDocument] = Seq(
    recorded(rich),
    recorded(Film("Lalka")),                                               // no original, year or popularity
    recorded(Film("Lalka", Some("Lalka"), year = Some(2025), popularity = Some(0.0))),
    recorded(rich).append("hit", searchHit),                               // the record wins over a search's hit
    new BsonDocument("hit", searchHit),                                    // a search's hit only
    new BsonDocument("record", BsonNull.VALUE).append("hit", searchHit),   // TMDB knows no such film; its hit stands
    new BsonDocument("record", BsonNull.VALUE),
    new BsonDocument("record", new BsonArray()).append("hit", searchHit),  // not a document: read as none
    new BsonDocument())

  "filmHit" should "read every film's hit as the frozen reading did" in {
    films.zipWithIndex.foreach { case (film, n) => withClue(s"film $n ($film): ")(TmdbStore.filmHit(1018, film) shouldBe frozen(1018, film)) }
  }

  it should "read a record's hit without decoding the whole record" in {
    assume(tools.ThreadAllocation.supported, "this JVM cannot say what a thread allocates")
    val film = recorded(rich)
    def allocated(hit: (Int, BsonDocument) => Option[Hit]): Long = {
      (1 to 2000).foreach(id => hit(id, film))
      tools.ThreadAllocation.of((1 to 20000).foreach(id => hit(id, film)))._2
    }
    val before = allocated(frozen)
    val now    = allocated(TmdbStore.filmHit)
    info(s"20000 hits allocated ${now / 1000} kB, the frozen reading ${before / 1000} kB")
    now should be < before / 5
  }
}
