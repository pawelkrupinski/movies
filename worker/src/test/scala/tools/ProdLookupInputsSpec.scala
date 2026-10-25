package tools

import models.{CinemaMovie, Country, Movie, Showtime}
import org.bson.{BsonArray, BsonDateTime, BsonDocument, BsonInt32, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._
import services.identity.adapters.{InMemoryTmdbDocuments, QuestionMemory, TmdbKind, TmdbStore}
import services.scrapes.{ArchivedScrape, SuccessfulScrape}

import java.nio.file.Files
import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters._

/** The live drift check's whole resolve answers its questions as the worker answers its model — prod's TMDB store, venue
 *  pages and question memory first, the recording only where the worker would ask live — over what a recording captured
 *  of them, and resolves the listings prod's model read: the accepted ones. */
class ProdLookupInputsSpec extends AnyFlatSpec with Matchers {
  private val clock    = Clock.fixed(Instant.parse("2026-10-25T00:30:00Z"), ZoneOffset.UTC)
  private val poland   = Country.Poland
  private val language = poland.language.toLanguageTag

  private def query(ids: Int*)  = new BsonDocument("ids", new BsonArray(ids.map(new BsonInt32(_)).asJava))
  private def hitDoc(hit: Hit)   = new BsonDocument("hit", TmdbStore.hitDoc(hit))
  private val prinz2026 = Hit(1001, "Der kleine Prinz", None, Some(2026), 6.0)
  private val prinz2015 = Hit(37627, "Der kleine Prinz", None, Some(2015), 3.5)
  private val prinzSearch = TmdbStore.titleSearchId(language, "Der kleine Prinz")
  private val prod = ProdLookupInputs.Captured(
    answers = Map(TmdbKind.Query -> Map(prinzSearch -> query(1001)), TmdbKind.Film -> Map("1001" -> hitDoc(prinz2026))), whole = Map.empty, pages = Nil)

  /** The recording's live answers, counting every question it is asked. */
  private final class Recording(answers: Map[CandidateQuery, Seq[Hit]]) extends IdentityLookups {
    val asked = new AtomicInteger
    def hasDetail(listing: Listing): Boolean                      = false
    def detail(listing: Listing): Answer[Option[DetailFacts]]     = Answer.Unknown
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]       = { asked.incrementAndGet(); answers.get(query).fold[Answer[Seq[Hit]]](Answer.Unknown)(Answer.Known(_)) }
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
  }
  private def lookupsOver(docs: ProdLookupInputs.Held, live: IdentityLookups) = ProdLookupInputs.lookups(docs, Nil, Nil, live, poland, clock)

  "the worker's lookups over a capture" should "answer from prod's store ahead of the recording's live answer, and the recording's on a gap" in {
    val live    = new Recording(Map(CandidateQuery.Title("Der kleine Prinz") -> Seq(prinz2015), CandidateQuery.Title("Neu") -> Seq(Hit(5, "Neu", None, None, 1.0))))
    val lookups = lookupsOver(new ProdLookupInputs.Held(prod), live)
    lookups.candidates(CandidateQuery.Title("Der kleine Prinz")).toOption.map(_.map(_.tmdbId)) shouldBe Some(Seq(1001))
    live.asked.get shouldBe 0
    lookups.candidates(CandidateQuery.Title("Neu")).toOption.map(_.map(_.tmdbId)) shouldBe Some(Seq(5))
  }

  it should "leave a gap prod's question memory holds back a gap, never asking the recording" in {
    val held = new ProdLookupInputs.Held(prod)
    new QuestionMemory(held, language, clock).unanswered(Seq(Question.Candidate(CandidateQuery.Title("Neu"))))
    val live = new Recording(Map(CandidateQuery.Title("Neu") -> Seq(Hit(5, "Neu", None, None, 1.0))))
    lookupsOver(held, live).candidates(CandidateQuery.Title("Neu")) shouldBe Answer.Unknown
    live.asked.get shouldBe 0
  }

  it should "read a listing's venue detail from prod's venue pages, as the model does" in {
    val page    = "http://kinopalacowe.pl/filmy/15308-festiwal-piec-smakow-morze-zote/"
    val listing = ModelRead.corpus(Seq(models.KinoPalacowe -> Seq(film(models.KinoPalacowe, "Festiwal Pięć Smaków: Morze żółte", Some(page)))),
      services.movies.TitleNormalizer.forCountry(poland)).head
    val enrichers = ProdLookupInputs.enrichersOf(poland, clock)
    val group     = enrichers.find(_.cinema == models.KinoPalacowe).map(_.detailGroup).getOrElse(fail("no Kino Pałacowe detail enricher"))
    val read      = new BsonDocument("_id", new BsonString(s"$group|$page")).append("group", new BsonString(group)).append("page", new BsonString(page))
      .append("readAt", new BsonDateTime(clock.millis())).append("director", new BsonArray(Seq(new BsonString("Na Hong-jin")).asJava))
      .append("releaseYear", new BsonInt32(2026))
    val lookups = ProdLookupInputs.lookups(new ProdLookupInputs.Held(prod.copy(pages = Seq(read))), Seq(read), enrichers, new Recording(Map.empty), poland, clock)
    val facts   = lookups.detail(listing).toOption.flatten.getOrElse(fail(s"no detail for $listing"))
    (facts.directors, facts.year) shouldBe (Seq("Na Hong-jin"), Some(2026))
  }

  "a capture" should "keep what a resolve read of prod's store, answers apart from whole documents, and write nothing to prod" in {
    val store = new InMemoryTmdbDocuments
    store.put(TmdbKind.Query, Seq(prinzSearch -> query(1001), TmdbStore.titleSearchId(language, "Unasked") -> query(9)))
    store.put(TmdbKind.Film, Seq("1001" -> hitDoc(prinz2026), "9" -> hitDoc(Hit(9, "Unasked", None, None, 1.0))))
    val listings = ModelRead.corpus(Seq(models.Helios -> Seq(film(models.Helios, "Der kleine Prinz", None))),
      services.movies.TitleNormalizer.forCountry(poland))
    val captured = ProdLookupInputs.capture(store, Nil, listings, Nil, poland, clock)
    captured.answers(TmdbKind.Query).keySet should contain(prinzSearch)
    captured.answers(TmdbKind.Film).keySet shouldBe Set("1001")
    captured.answers.values.flatMap(_.keys) should not contain TmdbStore.titleSearchId(language, "Unasked")
    store.size(TmdbKind.Marker) shouldBe 0
  }

  it should "read back as written, and be none for a recording made before it" in {
    val root = Files.createTempDirectory("recording")
    ProdLookupInputs.readFrom(root, "de") shouldBe None
    val whole = ProdLookupInputs.Captured(prod.answers, Map(TmdbKind.Marker -> Map("unanswered|x" -> new BsonDocument("fetchedAt", new org.bson.BsonInt64(5L)))),
      Seq(new BsonDocument("_id", new BsonString("g|p"))))
    ProdLookupInputs.write(root, "de", whole) shouldBe ProdLookupInputs.path(root, "de")
    ProdLookupInputs.readFrom(root, "de") shouldBe Some(whole)
  }

  "the listings prod's model read" should "be each venue's accepted listing, the recording's scrape only where it accepted none" in {
    val scraped  = Seq(venue(models.Helios, "Nowy, wstrzymany"), venue(models.Multikino, "Film Multikina"))
    val accepted = Seq(venue(models.Helios, "Stary, przyjęty"), venue(models.KinoMuza, "Film Muzy"), venue(models.Rialto))
    ProdListings.merged(scraped, accepted).map(row => row.cinema -> row.films.map(_.movie.title)) shouldBe
      Seq(models.Helios -> Seq("Stary, przyjęty"), models.Multikino -> Seq("Film Multikina"), models.KinoMuza -> Seq("Film Muzy"))
  }

  private def film(cinema: models.Cinema, title: String, page: Option[String]) =
    CinemaMovie(Movie(title), cinema, None, page, None, Nil, Nil, Seq(Showtime(LocalDateTime.of(2026, 10, 26, 18, 0), None, None, Nil)))
  private def venue(cinema: models.Cinema, titles: String*) = ArchivedScrape(cinema, None,
    Some(SuccessfulScrape(Instant.parse("2026-10-24T06:00:00Z"), listingComplete = true, films = titles.map(film(cinema, _, None)))), None)
}
