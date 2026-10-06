package services.review

import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.CatalogueId
import services.movies.ListingKey

/** Catalogue ids in every shape a document has carried them, and a feed catalogue's facts kept apart from the venue's. */
class CatalogueProvenanceSpec extends AnyFlatSpec with Matchers {

  "catalogue ids" should "read from a map, one {source, id}, a list of either or of strings, or one string" in {
    ListingFeed.catalogueIdsOf(BsonDocument.parse("""{"flicks": "29423", "cc": 7}""")) shouldBe
      Seq(CatalogueId("cc", "7"), CatalogueId("flicks", "29423"))
    ListingFeed.catalogueIdsOf(BsonDocument.parse("""{"source": "webedia", "id": "279943"}""")) shouldBe Seq(CatalogueId("webedia", "279943"))
    ListingFeed.catalogueIdsOf(new BsonArray(java.util.List.of(BsonDocument.parse("""{"source": "flicks", "id": "7842"}"""),
      new BsonString("webedia:1"), new BsonInt32(3)))) shouldBe Seq(CatalogueId("flicks", "7842"), CatalogueId("webedia", "1"))
    ListingFeed.catalogueIdsOf(new BsonString("flicks:11993")) shouldBe Seq(CatalogueId("flicks", "11993"))
    ListingFeed.catalogueIdsOf(new BsonString("11993")) shouldBe empty
    ListingFeed.catalogueIdsOf(null) shouldBe empty
  }

  "a listing whose facts a feed catalogue copied" should "be told apart, and raise no venue contradiction" in {
    val key  = ListingKey.Published("Planken Lichtspiele Mannheim", "Queen", Some(2020), Nil)
    val slot = Some(SlotFacts(VenueFacts(year = Some(2020), directors = Seq("Someone")), java.time.Instant.EPOCH))
    val fed  = MemberView(key, slot, None, Some(ListingFeed(Seq(CatalogueId("webedia", "279943")), 1, None, None)))
    val own  = MemberView(key, slot, None, Some(ListingFeed(Seq(CatalogueId("flicks", "1")), 1, None, None)))
    fed.factsFromCatalogue shouldBe true
    own.factsFromCatalogue shouldBe false
    val film = FilmFacts(FilmRef.tmdb(519465), Some("Queen of Hearts"), Some(1990), Seq("May el-Toukhy"))
    FactCheck.warnings(Seq(fed.member), film) shouldBe empty
    FactCheck.warnings(Seq(own.member), film) should not be empty
    MemberView(ListingKey.Native("Kino", "https://www.kinoprogramm.com/kinofilm/x-1", "X"), slot, None, None).factsFromCatalogue shouldBe true
  }
}
