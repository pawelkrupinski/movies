package integration

import controllers.ReviewController
import models.Country
import org.mongodb.scala.ObservableFuture
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonArray, BsonDateTime, BsonDocument, BsonInt32, BsonString}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.Mode
import play.api.libs.json.Json
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}
import services.identity.{MongoIdentityModelStore, ResolverDecisionBson}
import services.movies.ListingKey
import services.review._
import tools.IsolatedMongoDatabase
import services.{DebugMirror => DebugMirrorCollections}

import java.nio.file.Files
import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.Await

/**
 * The review pages over REAL Mongo: the mirror-shaped collections written as the worker writes them
 * (families by the decision codec, slot rows keyed by their listing key), read back by
 * [[MongoReviewSource]], and the answers kept in a [[MongoReviewAnswerStore]] — an answered cluster
 * leaves the queue, and stays out after a fresh store reads the history back.
 */
class ReviewPagesIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {
  import ReviewFixtures._

  private val mirror  = IsolatedMongoDatabase.open(mongoTarget, "review-mirror")
  private val review  = IsolatedMongoDatabase.open(mongoTarget, "review-answers")
  private val imports = IsolatedMongoDatabase.open(mongoTarget, "review-import")
  private val ordered = IsolatedMongoDatabase.open(mongoTarget, "review-order")
  private val now     = Instant.parse("2026-10-06T10:00:00Z")
  private val clock   = Clock.fixed(now, ZoneOffset.UTC)

  override protected def afterAll(): Unit = try { mirror.drop(); review.drop(); imports.drop(); ordered.drop() } finally super.afterAll()

  private def insert(collection: String, docs: BsonDocument*): Unit =
    Await.result(mirror.database.getCollection[Document](collection).insertMany(docs.map(Document(_))).toFuture(), tools.SpecTimeouts.Io): Unit

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    // one family holding the held (unmatched) cluster and the matched one, one holding the vetoed cluster
    insert(MongoIdentityModelStore.FamiliesCollection,
      new BsonDocument("_id", BsonString("f1")).append("decisions", BsonArray.fromIterable(Seq(heldDecision, matchedDecision).map(ResolverDecisionBson.encode))),
      new BsonDocument("_id", BsonString("f2")).append("decisions", BsonArray.fromIterable(Seq(vetoedDecision).map(ResolverDecisionBson.encode))))
    insert("movie_slots",
      new BsonDocument("_id", BsonString("m1\u001fKino Opalenica")).append("filmId", BsonString("m1")).append("slotKey", BsonString("Kino Opalenica"))
        .append("listingKey", BsonString(ListingKey.serialised(Held))).append("updatedAt", BsonDateTime(now.minusSeconds(7200).toEpochMilli))
        .append("slot", new BsonDocument("title", BsonString("Franz Kafka")).append("releaseYear", BsonInt32(2025))
          .append("director", BsonArray.fromIterable(Seq(BsonString("Agnieszka Holland")))).append("synopsis", BsonString("Biografia Kafki."))),
      new BsonDocument("_id", BsonString("m2\u001fKino Bajka")).append("filmId", BsonString("m2")).append("slotKey", BsonString("Kino Bajka"))
        .append("listingKey", BsonString(ListingKey.serialised(Matched))).append("updatedAt", BsonDateTime(now.minusSeconds(3600).toEpochMilli))
        .append("slot", new BsonDocument("title", BsonString("Klondike"))),
      new BsonDocument("_id", BsonString("m3\u001fTMDB")).append("filmId", BsonString("m3")).append("slotKey", BsonString("TMDB"))
        .append("updatedAt", BsonDateTime(now.toEpochMilli))
        .append("slot", new BsonDocument("title", BsonString("Franz")).append("releaseYear", BsonInt32(2025))
          .append("director", BsonArray.fromIterable(Seq(BsonString("Agnieszka Holland"))))))
    insert("movies", new BsonDocument("_id", BsonString("m3")).append("tmdbId", BsonInt32(1157322)).append("imdbId", BsonString("tt22963134"))
      .append("filmwebUrl", BsonString("https://www.filmweb.pl/film/Franz+Kafka-2025-10008278")))
    insert("identity_listings", new BsonDocument("_id", BsonString("Kino Opalenica")).append("films", BsonArray.fromIterable(Seq(
      new BsonDocument("movie", new BsonDocument("title", BsonString("Franz Kafka")).append("rawTitle", BsonString("FRANZ KAFKA")))
        .append("externalIds", new BsonDocument("bilety24", BsonString("165208"))).append("posterUrl", BsonString("https://b24/kafka.jpg"))
        .append("showtimes", BsonArray.fromIterable(Seq(new BsonDocument("dateTime", BsonDateTime(Instant.parse("2026-10-10T18:00:00Z").toEpochMilli))))),
      new BsonDocument("movie", new BsonDocument("title", BsonString("Other"))).append("showtimes", BsonArray())))))
    insert("venue_pages", new BsonDocument("_id", BsonString("b24|" + Held.nativeId)).append("page", BsonString(Held.nativeId))
      .append("runtimeMinutes", BsonInt32(127)).append("readAt", BsonDateTime(now.toEpochMilli)))
  }

  private val source = new MongoReviewSource(mirror.database)

  "the mirror reads" should "find each listing's own facts, feed and page, and the film's record by its TMDB id" in {
    source.decisions(unmatchedOnly = true).map(_.members) should contain theSameElementsAs Seq(Seq(Held), Seq(Matched), Seq(Vetoed))
    source.decisions(unmatchedOnly = false) should have size 3
    source.slots(Seq(ListingKey.serialised(Held))).values.map(_.facts.synopsis) shouldBe Seq(Some("Biografia Kafki."))
    source.updatedSince(now.minusSeconds(5400)).keySet shouldBe Set(ListingKey.serialised(Matched))
    source.feeds(Seq("Kino Opalenica" -> "FRANZ KAFKA")) shouldBe
      Map(("Kino Opalenica", "FRANZ KAFKA") -> ListingFeed(Seq(services.identity.CatalogueId("bilety24", "165208")), 1, Some("2026-10-10 18:00"), Some("2026-10-10 18:00"),
        Some("https://b24/kafka.jpg")))
    source.venuePages(Seq(Held.nativeId)).values.map(_.runtime) shouldBe Seq(Some(127))
    source.films(Seq(1157322, 42)) shouldBe Map(1157322 ->
      FilmCard(1157322, Some("tt22963134"), Some("Franz"), None, Some(2025), Seq("Agnieszka Holland"), None, None, None))
  }

  "a candidate's TMDB record" should "carry the poster the poster corroboration hashed it from, the first of its paths" in {
    import services.identity.PosterAnswers
    import services.identity.agreement.AgreementStage.PosterQuestion
    insert(DebugMirrorCollections.TmdbFilms,
      new BsonDocument("_id", BsonString("1599768")).append("record", new BsonDocument("title", BsonString("Ghost School")).append("year", BsonInt32(2026))),
      new BsonDocument("_id", BsonString("603")).append("hit", new BsonDocument("title", BsonString("The Matrix"))))
    // as the worker's PosterAnswerStore files a film's posters: the hashes, and the TMDB paths beside them
    insert(DebugMirrorCollections.FamilyAnswers, new BsonDocument("_id", BsonString(PosterAnswers.idOf(PosterQuestion.Film(1599768))))
      .append("hashes", BsonArray.fromIterable(Seq(org.bson.BsonInt64(7L), org.bson.BsonInt64(9L))))
      .append(PosterAnswers.Paths, BsonArray.fromIterable(Seq(BsonString("/ghost-pl.jpg"), BsonString("/ghost-en.jpg"))))
      .append("fetchedAt", org.bson.BsonInt64(now.toEpochMilli)))
    val records = source.filmRecords(Seq(1599768, 603))
    records(1599768).poster shouldBe Some(s"${PosterAnswers.FilmPosterBase}/ghost-pl.jpg")
    records(603).poster shouldBe None                       // hashed before paths were filed, or never: no poster
  }

  "the corpus's film links" should "tie a Filmweb id to the TMDB and IMDb ids of the same record" in {
    val franz = Set(FilmRef.tmdb(1157322), FilmRef("imdb", "tt22963134"), FilmRef("filmweb", "10008278"))
    source.filmLinks(Seq(FilmRef("filmweb", "10008278"))) shouldBe Seq(franz)
    source.filmLinks(Seq(FilmRef.tmdb(1157322), FilmRef("rt", "dolly"))) shouldBe Seq(franz)
    source.filmLinks(Seq(FilmRef("filmweb", "8278"))) shouldBe empty      // a suffix of the id is not the id
    FilmIdentity.of(source.filmLinks(Seq(FilmRef("filmweb", "10008278")))).provablyDifferent(FilmRef("filmweb", "10008278"), FilmRef.tmdb(2)) shouldBe true
  }

  "an answered cluster" should "leave the queue, and stay out for a fresh store reading the history back" in {
    val answers    = new ReviewAnswers(new MongoReviewAnswerStore(review.database))
    def controller(a: ReviewAnswers) = new ReviewController(Helpers.stubControllerComponents(), Mode.Dev, Map(Country.Poland -> source), a,
      Files.createTempFile("labels", ".tsv"), clock)
    val c          = controller(answers)
    contentAsString(c.queue(Some("pl"), 60, false)(FakeRequest())) should include("FRANZ KAFKA")
    val card = ReviewCards.build(source, Seq(ReviewCluster.of(Country.Poland, heldDecision) -> None), Nil, new ReviewAnswers.Index(Nil))
      .head.payload(ReviewPage.Queue)
    status(c.answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "event")))) shouldBe OK

    val reread = new ReviewAnswers(new MongoReviewAnswerStore(review.database))
    reread.current().map(a => (a.title, a.verdict, a.shown.map(_.ref.render))) shouldBe Seq(("FRANZ KAFKA", ReviewVerdict.Event, Some("tmdb:1157322")))
    val page = contentAsString(controller(reread).queue(Some("pl"), 60, false)(FakeRequest()))
    page should not include "FRANZ KAFKA"
    page should include("Macbeth")
    page should include("1 answered hidden")

    status(c.answer()(FakeRequest().withBody(Json.obj("card" -> card, "verdict" -> "undo")))) shouldBe OK
    reread.current() shouldBe empty
    reread.history().map(_.verdict) shouldBe Seq(ReviewVerdict.Event, ReviewVerdict.Undo)
  }

  "the stored history" should "run in the order of the answers' own times, not the wall clock's" in {
    val store  = new MongoReviewAnswerStore(ordered.database)
    val member = ReviewMember("Kino A", "Film", None)
    def answered(verdict: ReviewVerdict, at: Instant) = ReviewAnswer(ReviewClusterId.of(Seq(member)), "pl", ReviewPage.Queue,
      verdict, None, None, "Film", Seq(member), "dev", at)
    store.append(answered(ReviewVerdict.Wrong, now))
    store.append(answered(ReviewVerdict.Right, now.minusSeconds(3600)))
    store.all().map(_.verdict) shouldBe Seq(ReviewVerdict.Right, ReviewVerdict.Wrong)
  }

  "importing the hand-built pages' answers" should "store each once" in {
    val store   = new ReviewAnswers(new MongoReviewAnswerStore(imports.database))
    val fixture = Files.createTempFile("all-answers", ".json")
    Files.copy(getClass.getResourceAsStream("/review/all-answers.json"), fixture, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
    ReviewLabelsCli.run(store, List("import", fixture.toString)) should startWith("imported 71 of 71 answers")
    ReviewLabelsCli.run(store, List("import", fixture.toString)) should startWith("imported 0 of 71 answers (71 already there)")
    store.history() should have size 71
    val labels = Files.createTempFile("labels", ".tsv")
    ReviewLabelsCli.run(store, List("export", labels.toString)) should include("added")
    LabelsTsv.read(labels) should not be empty
  }
}
