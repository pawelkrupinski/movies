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

  "a card's facts" should "merge what the venues say, the catalogue's copied claims in a block of their own" in {
    val fed = MemberView(ListingKey.Published("Planken Lichtspiele Mannheim", "Queen", Some(2020), Nil),
      Some(SlotFacts(VenueFacts(year = Some(2020), directors = Seq("Someone"), runtime = Some(99)), java.time.Instant.EPOCH)), None,
      Some(ListingFeed(Seq(CatalogueId("webedia", "279943")), 2, Some("2026-10-07 18:00"), Some("2026-10-07 20:00"))))
    val own = MemberView(ListingKey.Native("Kino Muza", "https://muza/queen", "Queen"),
      Some(SlotFacts(VenueFacts(originalTitle = Some("Dronningen"), cast = (1 to 9).map(i => s"Actor $i")), java.time.Instant.EPOCH)),
      Some(VenueFacts(year = Some(2019), directors = Seq("May el-Toukhy"), runtime = Some(127), countries = Seq("DK"))),
      Some(ListingFeed(Seq(CatalogueId("bilety24", "1")), 3, Some("2026-10-06 18:00"), Some("2026-10-09 18:00"))))
    val card = ReviewCard(ReviewCluster(models.Country.Germany, Seq(fed.key, own.key), None, 0.2,
      services.identity.ResolverDecision.Basis.BelowThreshold, Nil, fallback = false, Nil), Seq(fed, own), Map.empty, Nil, None, None)
    card.venueSays shouldBe Seq("Original title" -> "Dronningen", "Year" -> "2019", "Director" -> "May el-Toukhy",
      "Cast" -> "Actor 1, Actor 2, Actor 3, Actor 4, Actor 5, Actor 6, Actor 7, Actor 8 …", "Runtime" -> "127 min",
      "Country" -> "DK", "Catalogue ids" -> "bilety24=1", "Screenings" -> "5 · 2026-10-06 18:00 → 2026-10-09 18:00")
    card.catalogueSays shouldBe Seq("Year" -> "2020", "Director" -> "Someone", "Runtime" -> "99 min", "Catalogue ids" -> "webedia=279943")
    ReviewCard(card.cluster, Seq(own), Map.empty, Nil, None, None).catalogueSays shouldBe empty
  }
}
