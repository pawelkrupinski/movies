package services.identity

import clients.TmdbClient
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{HttpStatusException, MutableClock, TestWiring}

import scala.collection.mutable
import scala.util.{Failure, Success}

/** TMDB's answers normalized as they are fetched: read back exactly as the client parses them, with
 *  one film, one person and one question each, and moved — announced — only when a value the
 *  resolver reads changes. */
class TmdbStoreSpec extends AnyFlatSpec with Matchers {

  private val film = 1018
  private val language = "pl-PL"

  private final class World {
    val clock   = new MutableClock(TestWiring.FixedInstant)
    val docs    = new InMemoryTmdbDocuments
    val store   = new TmdbStore(docs, clock)
    val changed = mutable.ArrayBuffer.empty[String]
    store.onChanged(key => changed += key)
    val normalizer = new TmdbNormalizer(store)
    def lookups = new StoredTmdbLookups(store, language, NoDetails, new ObservationReads)
  }

  private object NoDetails extends IdentityLookups {
    def hasDetail(l: Listing) = false
    def detail(l: Listing)    = Answer.Known(None)
    def candidates(q: CandidateQuery) = Answer.Unknown
    def film(id: Int)                 = Answer.Unknown
  }

  private def local(popularity: Double = 8.8983) =
    s"""{"id":$film,"title":"Mulholland Drive","original_title":"Mulholland Drive","release_date":"2001-06-06","runtime":146,
       |"popularity":$popularity,"imdb_id":"tt0166924","production_countries":[{"iso_3166_1":"FR","name":"France"}],"origin_country":["US"],
       |"overview":"…","genres":[{"id":18,"name":"Dramat"}],"credits":{"cast":[{"name":"Naomi Watts"}],
       |"crew":[{"job":"Director","name":"David Lynch"},{"job":"Writer","name":"David Lynch"}]},"release_dates":{"results":[]}}""".stripMargin
  private val english =
    s"""{"id":$film,"title":"Mulholland Drive","popularity":8.8983,"alternative_titles":{"titles":[{"iso_3166_1":"US","title":"Mulholland Dr."}]}}"""
  private def url(append: String, lang: String) = s"https://api.themoviedb.org/3/movie/$film?language=$lang&append_to_response=$append"

  "a film's record" should "read back as the client's own parse of its two responses, popularity to its bucket" in {
    val w = new World
    val observed = new NormalizingHttpFetch(new FakeHttpFetch("08-06-2026", strict = true), w.normalizer)
    val client   = new TmdbClient(observed, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => ())
    val parsed   = client.identityRecord(film)
    parsed shouldBe defined
    w.lookups.film(film) shouldBe Answer.Known(parsed.map(f => f.copy(popularity = f.popularity.map(p => PopularityBucket.representative(PopularityBucket.of(p))))))
    // What a film document holds: the two cut-down partials and the record — never the body's cast or synopsis.
    val stored = w.docs.get(TmdbKind.Film, Seq(film.toString))(film.toString)
    stored.keySet should contain allOf ("local", "english", "record")
    stored.toJson should not include "overview"
  }

  it should "not move, nor wake anyone, when a re-fetch changes only popularity within its bucket" in {
    val w = new World
    w.normalizer.filed("GET", url("credits,release_dates", language), Success(local()))
    w.normalizer.filed("GET", url("alternative_titles", "en-US"), Success(english))
    val first = w.docs.get(TmdbKind.Film, Seq(film.toString))
    w.changed.clear()
    w.clock.advance(java.time.Duration.ofDays(1))
    w.normalizer.filed("GET", url("credits,release_dates", language), Success(local(popularity = 9.7))) // 8.9 → 9.7: bucket 3 both
    w.changed shouldBe empty
    w.docs.get(TmdbKind.Film, Seq(film.toString)) shouldBe first                                          // changedAt too
    w.normalizer.filed("GET", url("credits,release_dates", language), Success(local(popularity = 17.0))) // bucket 4
    w.changed shouldBe Seq(TmdbStore.keyOf(TmdbKind.Film, film.toString))
  }

  "a search's hit" should "stand in for a film until its record arrives, then give way to it" in {
    val w = new World
    val search = """{"results":[{"id":1018,"title":"Mulholland Dr.","original_title":"Mulholland Drive","release_date":"2001-06-06","popularity":8.9}]}"""
    w.normalizer.filed("GET", s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=Mulholland", Success(search))
    w.lookups.candidates(CandidateQuery.Title("Mulholland")) shouldBe
      Answer.Known(Seq(Hit(film, "Mulholland Dr.", Some("Mulholland Drive"), Some(2001), PopularityBucket.representative(3))))
    w.lookups.film(film) shouldBe Answer.Unknown                                                    // no record yet: the fill's question
    w.normalizer.filed("GET", url("credits,release_dates", language), Success(local()))
    w.normalizer.filed("GET", url("alternative_titles", "en-US"), Success(english))
    w.docs.get(TmdbKind.Film, Seq(film.toString))(film.toString).containsKey("hit") shouldBe false
    w.lookups.candidates(CandidateQuery.Title("Mulholland")).toOption.get.map(_.title) shouldBe Seq("Mulholland Drive")
  }

  "a 404" should "be read as the client reads it: a record's missing half, a person with no credits; a search's is no answer" in {
    val w = new World
    w.normalizer.filed("GET", url("credits,release_dates", language), Failure(new HttpStatusException(404, "GET", "…", None)))
    w.normalizer.filed("GET", url("alternative_titles", "en-US"), Success(english))
    w.lookups.film(film).toOption.flatten.map(_.directors) shouldBe Some(Some(Nil))                 // `{"crew":[]}`: known, none
    w.normalizer.filed("GET", s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=Nic",
      Failure(new HttpStatusException(404, "GET", "…", None)))
    w.lookups.candidates(CandidateQuery.Title("Nic")) shouldBe Answer.Unknown
    w.normalizer.filed("GET", s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=Nic",
      Failure(new java.io.IOException("reset")))
    w.lookups.candidates(CandidateQuery.Title("Nic")) shouldBe Answer.Unknown
  }

  "a model over the store" should "take up with every question a gap, decide once the fill's answers are normalized in, and sleep through popularity drift" in {
    import models.{Movie, CinemaMovie, Multikino, Showtime}
    val w        = new World
    val reads    = new ObservationReads
    val titles   = services.movies.SingleCountryNormalizer.titleNormalizer
    val listings = Listing.distinct(Listing.all(Seq(Multikino -> Seq(CinemaMovie(Movie("Lalka", releaseYear = Some(2025)), Multikino, None,
      Some("Multikino/Lalka"), None, Nil, Nil, Seq(Showtime(java.time.LocalDateTime.of(2026, 10, 1, 18, 0), None))))), titles))
    var model  = Option.empty[IncrementalResolver]
    val service = new IdentityModelService(
      () => { val m = new IncrementalResolver(new TrackedLookups(new StoredTmdbLookups(w.store, language, NoDetails, reads), reads,
        Some(java.util.concurrent.Executors.newFixedThreadPool(2))), titles, IdentityCalibration.resolver); model = Some(m); m },
      reads, () => listings, titles, scala.concurrent.duration.Duration(1, "second"), java.util.concurrent.Executors.newSingleThreadScheduledExecutor())
    w.store.onChanged(service.observed)
    service.takeUp()
    model.get.decisions.flatMap(_.film) shouldBe empty
    val gaps = model.get.gaps
    gaps.queries should not be empty

    // The fill: each gap asked of TMDB, the answers normalized in as they arrive.
    def record(id: Int, year: Int, director: String, popularity: Double) = Seq(
      s"https://api.themoviedb.org/3/movie/$id?language=$language&append_to_response=credits,release_dates" ->
        s"""{"id":$id,"title":"Lalka","original_title":"Lalka","release_date":"$year-01-01","popularity":$popularity,"credits":{"crew":[{"job":"Director","name":"$director"}]}}""",
      s"https://api.themoviedb.org/3/movie/$id?language=en-US&append_to_response=alternative_titles" ->
        s"""{"id":$id,"title":"The Doll","popularity":$popularity,"alternative_titles":{"titles":[]}}""")
    def fill(popularity: Double): Unit = {
      gaps.queries.collect { case CandidateQuery.Title(text) => text }.foreach { text =>
        w.normalizer.filed("GET", s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=${java.net.URLEncoder.encode(text, "UTF-8")}",
          Success(s"""{"results":[{"id":2,"title":"Lalka","original_title":"Lalka","release_date":"2025-09-19","popularity":$popularity},
                     |{"id":1,"title":"Lalka","original_title":"Lalka","release_date":"1968-02-02","popularity":3.5}]}""".stripMargin))
      }
      (record(1, 1968, "Wojciech Has", 3.5) ++ record(2, 2025, "Maciej Kawalski", popularity)).foreach { case (u, b) => w.normalizer.filed("GET", u, Success(b)) }
    }
    fill(popularity = 12.4)
    service.drain() shouldBe defined
    model.get.decisions.flatMap(_.film) shouldBe Seq(2)
    fill(popularity = 13.1)                                                    // tomorrow's re-fetch: same bucket
    service.drain() shouldBe None
  }

  "the backfill" should "move the raw TMDB answers the observation store holds into the normalized store, 404s included, once" in {
    import services.observations.{LookupAnswer, LookupQuery, ObservationStore}
    val w   = new World
    val obs = ObservationStore.inMemory(w.clock)
    obs.observeLookup(LookupQuery.of("GET", url("credits,release_dates", language)), LookupAnswer.Body(local()))
    obs.observeLookup(LookupQuery.of("GET", url("alternative_titles", "en-US")), LookupAnswer.Body(english))
    obs.observeLookup(LookupQuery.of("GET", "https://api.themoviedb.org/3/person/5/movie_credits?language=pl-PL"),
      LookupAnswer.Failed(Some(404), "GET", "HTTP 404"))
    obs.observeLookup(LookupQuery.of("GET", "https://www.metacritic.com/movie/lalka/"), LookupAnswer.Body("<html/>"))
    val backfill = new TmdbStoreBackfill(obs, w.normalizer, w.docs, w.clock)
    backfill.ensure()
    w.lookups.film(film).toOption.flatten.map(_.directors) shouldBe Some(Some(Seq("David Lynch")))
    w.docs.get(TmdbKind.Person, Seq("5"))("5").getArray("directed").size shouldBe 0            // 404: a person with no credits
    val filed = w.changed.size
    backfill.ensure()                                                                             // done once: marked
    w.changed.size shouldBe filed
  }

  "the popularity bucket" should "be the measure's own, and give itself back from its representative" in {
    (-10 to 20).foreach(b => PopularityBucket.of(PopularityBucket.representative(b)) shouldBe b)
    PopularityBucket.of(0.0) shouldBe -10
    PopularityBucket.of(8.8983) shouldBe 3
    PopularityBucket.of(16.0) shouldBe 4
  }
}
