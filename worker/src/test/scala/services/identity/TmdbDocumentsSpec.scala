package services.identity

import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonNull, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The in-memory backend under the normalized store's storage contract. */
class TmdbDocumentsSpec extends TmdbDocumentsBehaviour {
  protected def newDocuments(): TmdbDocuments = new InMemoryTmdbDocuments
}

/** What any backend of the normalized TMDB store keeps: `TmdbDocumentsSpec` (in memory) and
 *  `MongoTmdbDocumentsIntegrationSpec` (Mongo) run the same cases. */
trait TmdbDocumentsBehaviour extends AnyFlatSpec with Matchers {
  protected def newDocuments(): TmdbDocuments

  private def film(title: String) = new BsonDocument("record", new BsonDocument("title", BsonString(title))
    .append("directors", new BsonArray(java.util.List.of(BsonString("Wojciech Has"))))).append("gone", BsonNull())
  private val search = new BsonDocument("ids", new BsonArray(java.util.List.of(BsonInt32(1018), BsonInt32(9))))

  "the normalized documents" should "give back what each kind kept, by id, nested as written, and nothing never kept" in {
    val d = newDocuments()
    d.put(TmdbKind.Film, Seq("1018" -> film("Lalka")))
    d.put(TmdbKind.Query, Seq("movie|pl-PL|Lalka" -> search))
    d.get(TmdbKind.Film, Seq("1018", "9")) shouldBe Map("1018" -> film("Lalka"))
    d.get(TmdbKind.Query, Seq("movie|pl-PL|Lalka")) shouldBe Map("movie|pl-PL|Lalka" -> search)
    d.get(TmdbKind.Person, Seq("1018")) shouldBe empty                           // kinds do not share ids
  }

  it should "keep only the latest value an id was put with, and not change what a reader already holds" in {
    val d = newDocuments()
    d.put(TmdbKind.Film, Seq("1018" -> film("Lalka")))
    val held = d.get(TmdbKind.Film, Seq("1018"))("1018")
    held.put("record", BsonNull())
    d.get(TmdbKind.Film, Seq("1018"))("1018") shouldBe film("Lalka")
    d.put(TmdbKind.Film, Seq("1018" -> film("Lalka (1968)")))
    d.get(TmdbKind.Film, Seq("1018"))("1018") shouldBe film("Lalka (1968)")
  }
}
