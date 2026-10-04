package services.identity

import tools.SpecTimeouts

import clients.TmdbClient
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{HttpStatusException, MutableClock, TestWiring}

import scala.collection.mutable
import scala.jdk.CollectionConverters._
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
    def lookups = new StoredTmdbLookups(store, language, UnansweredTmdbLookups, new ObservationReads)
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

  // The normalizer parses each response to file it, then the client parses the very same body: shared,
  // a film record's two responses are parsed once each, and what is filed and answered is unchanged.
  it should "read back the same when the normalizer and the client share their parses" in {
    def read(shared: Boolean) = {
      val w        = new World
      val bodies   = new tools.JsonBodies
      val observed = new NormalizingHttpFetch(new FakeHttpFetch("08-06-2026", strict = true),
                                              if (shared) new TmdbNormalizer(w.store, bodies) else w.normalizer)
      val client   = new TmdbClient(observed, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => (),
                                    bodies = if (shared) bodies else new tools.JsonBodies)
      (client.identityRecord(film), w.lookups.film(film), w.docs.get(TmdbKind.Film, Seq(film.toString)))
    }
    read(shared = true) shouldBe read(shared = false)
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

  // A refreshed search only names its films: it is not a fetch of their records. Re-stamping each named film
  // (`fetchedAt`) once it was a day old made every re-asked question ~20 film writes — DE's tmdb_films took
  // ~4.8 writes/s, the fill's whole pace, for records that had not changed (2026-10-04).
  it should "not write a film whose record it names again, however long ago that record was fetched" in {
    val w = new World
    w.normalizer.filed("GET", url("credits,release_dates", language), Success(local()))
    w.normalizer.filed("GET", url("alternative_titles", "en-US"), Success(english))
    val before = w.docs.get(TmdbKind.Film, Seq(film.toString))(film.toString)
    w.clock.advance(java.time.Duration.ofMillis((TmdbStore.RenewEvery * 3).toMillis))
    val search = """{"results":[{"id":1018,"title":"Mulholland Dr.","original_title":"Mulholland Drive","release_date":"2001-06-06","popularity":8.9}]}"""
    w.normalizer.filed("GET", s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=Mulholland", Success(search))
    w.docs.get(TmdbKind.Film, Seq(film.toString))(film.toString) shouldBe before
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
      () => { val m = new IncrementalResolver(new TrackedLookups(new StoredTmdbLookups(w.store, language, UnansweredTmdbLookups, reads), reads,
        Some(java.util.concurrent.Executors.newFixedThreadPool(2))), titles, IdentityCalibration.resolver); model = Some(m); m },
      reads, () => listings, titles, scala.concurrent.duration.Duration(1, "second"), java.util.concurrent.Executors.newSingleThreadScheduledExecutor(), clock = _root_.tools.SpecClock.Pinned)
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

  /** A take-up asks tens of thousands of questions. Each read one document at a time — its search,
   *  then every film it names — as its own round-trip: 40.5k asks took 21 s of a UK restore's 28 s
   *  in production, against a fraction of a second for the same documents read in batches. The
   *  prefetch the model's lookups run must reach the store's own batched prefetch. */
  "a prefetch through the model's lookups" should "read the store in a few batches, not a round-trip per document" in {
    val w     = new World
    val gets  = new java.util.concurrent.atomic.AtomicInteger()
    val docs  = new TmdbDocuments {
      def get(kind: TmdbKind, ids: Seq[String]) = { gets.incrementAndGet(); w.docs.get(kind, ids) }
      def put(kind: TmdbKind, d: Seq[(String, org.bson.BsonDocument)]) = w.docs.put(kind, d)
    }
    val titles = (1 to 30).map(i => s"Film $i")
    titles.zipWithIndex.foreach { case (text, i) =>
      w.normalizer.filed("GET", s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=${java.net.URLEncoder.encode(text, "UTF-8")}",
        Success(s"""{"results":[{"id":${100 + i},"title":"$text","original_title":"$text","release_date":"2020-01-01","popularity":5.0},
                   |{"id":${200 + i},"title":"$text","original_title":"$text","release_date":"1990-01-01","popularity":1.0}]}""".stripMargin))
    }
    val reads   = new ObservationReads
    val store   = new TmdbStore(docs, w.clock)
    val lookups = new TrackedLookups(new StoredTmdbLookups(store, language, UnansweredTmdbLookups, reads), reads,
      Some(java.util.concurrent.Executors.newFixedThreadPool(4)))
    val queries = titles.map(CandidateQuery.Title(_))

    lookups.prefetch(queries, Nil, Nil)
    queries.map(lookups.candidates).map(_.toOption.map(_.map(_.tmdbId).toSet)) shouldBe
      titles.indices.map(i => Some(Set(100 + i, 200 + i)))
    gets.get should be <= 4
  }

  /** …and holds the documents no longer than the asks that read them. Kept until the next
   *  prefetch, a slice's searches and every film they name sat in the heap through the slice's
   *  whole build and the resolves after it — UK's take-up then spent 72% of its time in full GCs. */
  it should "drop the documents it fetched once the prefetch's own asks are answered" in {
    val w       = new World
    val titles  = (1 to 5).map(i => s"Film $i")
    titles.zipWithIndex.foreach { case (text, i) =>
      w.normalizer.filed("GET", s"https://api.themoviedb.org/3/search/movie?language=$language&include_adult=false&query=${java.net.URLEncoder.encode(text, "UTF-8")}",
        Success(s"""{"results":[{"id":${100 + i},"title":"$text","original_title":"$text","release_date":"2020-01-01","popularity":5.0}]}"""))
    }
    val reads   = new ObservationReads
    val stored  = new StoredTmdbLookups(w.store, language, UnansweredTmdbLookups, reads)
    val lookups = new TrackedLookups(stored, reads, Some(java.util.concurrent.Executors.newFixedThreadPool(2)))
    val queries = titles.map(CandidateQuery.Title(_))

    lookups.prefetch(queries, Nil, Nil)
    stored.heldDocuments shouldBe 0
    queries.map(lookups.candidates).map(_.toOption.map(_.map(_.tmdbId))) shouldBe titles.indices.map(i => Some(Seq(100 + i)))
  }

  /** A film document carries the two partial responses its record was parsed from beside the
   *  record: ~650 of ~1,000 bytes that no answer reads. A take-up fetched and decoded them for every
   *  film it named — UK's store batches took 12.4 s of a 21 s context. Answers read only what they use. */
  "the model's lookups" should "read a film's answer fields, of its partial responses only the IMDb id" in {
    val w = new World
    val observed = new NormalizingHttpFetch(new FakeHttpFetch("08-06-2026", strict = true), w.normalizer)
    new TmdbClient(observed, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => ()).identityRecord(film) shouldBe defined
    val wholeFilmReads = new java.util.concurrent.atomic.AtomicInteger()
    val docs = readingThrough(w.docs)(kind => if (kind == TmdbKind.Film) { wholeFilmReads.incrementAndGet(); () })
    val lookups = new StoredTmdbLookups(new TmdbStore(docs, w.clock), language, UnansweredTmdbLookups, new ObservationReads)
    lookups.prefetch(Nil, Seq(film), Nil)
    lookups.film(film) shouldBe w.lookups.film(film)
    lookups.film(film).toOption.flatten shouldBe defined
    wholeFilmReads.get shouldBe 0
    val answer = w.docs.answers(TmdbKind.Film, Seq(film.toString))(film.toString)
    Seq("local", "english").flatMap(partial => Option(answer.get(partial))).flatMap(_.asDocument.keySet.asScala).toSet shouldBe Set("imdb_id")
  }

  // A record filed before records carried IMDb's number holds none; the answer still names it, off the partials'
  // `imdb_id` — the number a no-match's lean is compared with a card's carried IMDb id by.
  "a film's record filed without its IMDb number" should "still answer with the number its responses name" in {
    val w = new World
    val observed = new NormalizingHttpFetch(new FakeHttpFetch("08-06-2026", strict = true), w.normalizer)
    new TmdbClient(observed, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => ()).identityRecord(film) shouldBe defined
    w.lookups.film(film).toOption.flatten.map(_.imdbNumber) shouldBe Some(166924)
    val filed = w.docs.get(TmdbKind.Film, Seq(film.toString))(film.toString)
    filed.getDocument("record").remove("imdbNumber")
    w.docs.put(TmdbKind.Film, Seq(film.toString -> filed))
    w.docs.get(TmdbKind.Film, Seq(film.toString))(film.toString).getDocument("record").containsKey("imdbNumber") shouldBe false
    w.lookups.film(film).toOption.flatten.map(_.imdbNumber) shouldBe Some(166924)
  }

  /** `docs`, with `onGet` run before each whole-document read — the read every write's compare makes. */
  private def readingThrough(docs: TmdbDocuments)(onGet: TmdbKind => Unit): TmdbDocuments = new TmdbDocuments {
    def get(kind: TmdbKind, ids: Seq[String]) = { onGet(kind); docs.get(kind, ids) }
    def put(kind: TmdbKind, d: Seq[(String, org.bson.BsonDocument)]) = docs.put(kind, d)
    override def answers(kind: TmdbKind, ids: Seq[String]) = docs.answers(kind, ids)
  }

  /** Run `writes` on threads of their own, all at once, and wait for every one — failing with the first that threw. */
  private def atOnce(writes: Seq[() => Unit]): Unit = {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(writes.size)
    try writes.map(write => pool.submit[Unit](() => write())).foreach(_.get(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS))
    finally { pool.shutdownNow(); () }
  }

  private def minimalOf(body: String): play.api.libs.json.JsValue = TmdbNormalizer.minimal(play.api.libs.json.Json.parse(body))

  /** A take-up of an empty store files every film it names through these writes, from 64 prefetch
   *  threads: one store-wide lock held across each write's read and write round-trips made them one
   *  at a time — the US convergence leg's take-up spent 138 s filing 40,950 film records serially. */
  "writes to different documents" should "not wait for each other" in {
    val w        = new World
    val bothRead = new java.util.concurrent.CyclicBarrier(2)
    // Each write's read waits for the other's: under one store-wide lock the second never arrives.
    val store = new TmdbStore(readingThrough(w.docs)(_ => { bothRead.await(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS); () }), w.clock)
    atOnce(Seq(film, film + 1).map(id => () => store.filmPartial(id, TmdbStore.Partial.Local, minimalOf(local()))))
    w.docs.get(TmdbKind.Film, Seq(film.toString, (film + 1).toString)).keySet shouldBe Set(film.toString, (film + 1).toString)
  }

  "a film's two partial responses" should "both land when they arrive at once" in {
    val w     = new World
    // Widen the window between a write's read and its write, where an unguarded pair loses one partial.
    val store = new TmdbStore(readingThrough(w.docs)(_ => Thread.sleep(50)), w.clock)
    atOnce(Seq(TmdbStore.Partial.Local -> local(), TmdbStore.Partial.English -> english)
      .map { case (partial, body) => () => store.filmPartial(film, partial, minimalOf(body)) })
    w.docs.get(TmdbKind.Film, Seq(film.toString))(film.toString).keySet should contain allOf ("local", "english", "record")
  }

  "the popularity bucket" should "be the measure's own, and give itself back from its representative" in {
    (-10 to 20).foreach(b => PopularityBucket.of(PopularityBucket.representative(b)) shouldBe b)
    PopularityBucket.of(0.0) shouldBe -10
    PopularityBucket.of(8.8983) shouldBe 3
    PopularityBucket.of(16.0) shouldBe 4
  }

  /** IMDb's suggestions for "Camino dla opornych", TMDB's find of their ids and IMDb's titles of each, as recorded. */
  private final class CaminoFetch extends tools.HttpFetch {
    private def fixture(path: String) = scala.io.Source.fromResource(path)(using scala.io.Codec.UTF8).mkString
    val posted = mutable.ArrayBuffer.empty[String]
    override def get(url: String): String =
      if (url.startsWith(services.enrichment.ImdbClient.SuggestionBase)) fixture("fixtures/imdb/suggestion_camino_dla_opornych.json")
      else if (url.contains("/find/tt39814688")) fixture("fixtures/tmdb/find_compostelle_by_imdb_id.json")
      else throw new HttpStatusException(404, "GET", url, None)
    override def post(url: String, body: String, contentType: String): String = {
      posted += body
      services.enrichment.ImdbClient.titlesQueryId(body) match {
        case Some("tt39814688") => fixture("fixtures/imdb/akas_compostelle_polish_title.json")
        case other              => throw new HttpStatusException(404, "POST", s"$url $other", None)
      }
    }
  }

  "an IMDb-titled question" should "find the film IMDb lists under the title in another language, and read back from the store as asked live" in {
    // "Camino dla opornych" is tt39814688's Polish title on IMDb, which displays it as "Santiago: The Camino Therapy";
    // TMDB knows the film only as "Compostelle".
    val w        = new World
    val fetch    = new NormalizingHttpFetch(new CaminoFetch, w.normalizer)
    val live     = new TmdbIdentityLookups(new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => ()),
      new services.enrichment.ImdbClient(fetch), Nil)
    val query    = CandidateQuery.ImdbTitled("Camino dla opornych")
    val answered = live.candidates(query)
    answered.toOption.map(_.map(_.tmdbId)) shouldBe Some(Seq(1404604))
    w.lookups.candidates(query).toOption.map(_.map(_.tmdbId)) shouldBe Some(Seq(1404604))
  }

  it should "be unknown to the store until IMDb's titles of a suggestion it does not display under the title are filed" in {
    val w = new World
    w.normalizer.filed("GET", services.enrichment.ImdbClient.suggestionUrl("Camino dla opornych"),
      Success(scala.io.Source.fromResource("fixtures/imdb/suggestion_camino_dla_opornych.json")(using scala.io.Codec.UTF8).mkString))
    w.lookups.candidates(CandidateQuery.ImdbTitled("Camino dla opornych")) shouldBe Answer.Unknown
  }

  /** IMDb's suggestions for "Snow Leopard", TMDB's finds of the two IMDb displays under that title (Pema Tseden's 2023
   *  film, which TMDB holds, and Lixing Wang's 2020 one, which it does not), IMDb's titles of the suggestions it displays
   *  otherwise, and IMDb's record of the 2020 film — as recorded 2026-10-04. */
  private class SnowLeopardFetch extends tools.HttpFetch {
    private def fixture(path: String) = scala.io.Source.fromResource(path)(using scala.io.Codec.UTF8).mkString
    override def get(url: String): String =
      if (url.startsWith(services.enrichment.ImdbClient.SuggestionBase)) fixture("fixtures/imdb/suggestion_snow_leopard.json")
      else if (url.contains("/find/tt21223152")) fixture("fixtures/tmdb/find_snow_leopard_2023.json")
      else if (url.contains("/find/tt13920372")) fixture("fixtures/tmdb/find_snow_leopard_none.json")
      else throw new HttpStatusException(404, "GET", url, None)
    override def post(url: String, body: String, contentType: String): String =
      (services.enrichment.ImdbClient.titlesQueryId(body), services.enrichment.ImdbClient.identityRecordId(body)) match {
        case (Some(tt @ ("tt31034190" | "tt0077847")), _) => fixture(s"fixtures/imdb/akas_$tt.json")
        case (_, Some("tt13920372"))                      => fixture("fixtures/imdb/identity_record_snow_leopard.json")
        case other                                        => throw new HttpStatusException(404, "POST", s"$url $other", None)
      }
  }

  "a fallback question" should "offer only the film IMDb lists under the title that TMDB has no record of, read back from the store as asked live" in {
    val w     = new World
    val fetch = new NormalizingHttpFetch(new SnowLeopardFetch, w.normalizer)
    val live  = new TmdbIdentityLookups(new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => ()),
      new services.enrichment.ImdbClient(fetch), Nil)
    val query = CandidateQuery.ImdbTitled("Snow Leopard")
    val wang  = FallbackIds.ofImdbId("tt13920372").get
    live.candidates(query).toOption.map(_.map(hit => (hit.tmdbId, hit.year))) shouldBe Some(Seq((wang, Some(2020))))
    w.lookups.candidates(query) shouldBe live.candidates(query)
    val record = live.film(wang)
    record.toOption.flatten.map(f => (f.title, f.year, f.directors, f.countries)) shouldBe
      Some(("Snow Leopard", Some(2020), Some(Seq("Lixing Wang")), Some(Seq("CN"))))
    w.lookups.film(wang) shouldBe record
  }

  it should "be unknown to the store until IMDb's record of the film is filed" in {
    val w     = new World
    val fetch = new NormalizingHttpFetch(new SnowLeopardFetch, w.normalizer)
    new TmdbIdentityLookups(new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => ()),
      new services.enrichment.ImdbClient(fetch), Nil).candidates(CandidateQuery.ImdbTitled("Snow Leopard"))
    w.lookups.film(FallbackIds.ofImdbId("tt13920372").get) shouldBe Answer.Unknown
  }

  // IMDb answering 404 for a title's titles or record is IMDb having no such title: the live client reads it as no
  // titles / no record, an answer. The normalizer filed nothing for a failed POST, so the store said Unknown for ever
  // where the live lookup had answered — the hard clusters' "normalized store answers as the recorded responses" once
  // their re-recording held such 404s (hc-pl "2D", hc-uk "It (1990)").
  it should "read IMDb's 404 on a title's titles or record from the store as the live lookup reads it" in {
    val w     = new World
    val fetch = new NormalizingHttpFetch(new SnowLeopardFetch {
      override def post(url: String, body: String, contentType: String): String =
        throw new HttpStatusException(404, "POST", url, None)
    }, w.normalizer)
    val live  = new TmdbIdentityLookups(new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => ()),
      new services.enrichment.ImdbClient(fetch), Nil)
    val query = CandidateQuery.ImdbTitled("Snow Leopard")
    val wang  = FallbackIds.ofImdbId("tt13920372").get
    val asked = live.candidates(query)
    asked.toOption shouldBe defined
    w.lookups.candidates(query) shouldBe asked
    val record = live.film(wang)
    record shouldBe Answer.Known(None)
    w.lookups.film(wang) shouldBe record
  }

  "IMDb's films under a title" should "be none when one of them has no TMDB record: the one found is not the only one" in {
    TmdbIdentityLookups.everyTitled(Seq(Seq(Hit(134673, "Renoir", None, Some(2012), 3.0)), Nil)) shouldBe empty
    TmdbIdentityLookups.everyTitled(Seq(Seq(Hit(1404604, "Compostelle", None, Some(2026), 2.0)))).map(_.tmdbId) shouldBe Seq(1404604)
  }

  "A title's IMDb suggestions" should "be a gap to both questions they answer when their read met one" in {
    // The hard-cluster replay answers an unrecorded suggestion URL 404 — IMDb's own "no such title", an empty list —
    // and counts the miss. Read once for the IMDb-titled question and kept, the second question took the empty list
    // as IMDb's answer without the miss counted: the store, which files nothing for a miss, said Unknown.
    var misses = 0L
    val fetch  = new tools.HttpFetch {
      override def get(url: String): String = { misses += 1; throw new HttpStatusException(404, "GET", url, None) }
      override def post(url: String, body: String, contentType: String): String = get(url)
    }
    val live = new TmdbIdentityLookups(new TmdbClient(fetch, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => ()),
      new services.enrichment.ImdbClient(fetch), Nil, new TmdbIdentityLookups.CountedGaps(() => misses))
    live.candidates(CandidateQuery.ImdbTitled("B-Movie")) shouldBe Answer.Unknown
    live.candidates(CandidateQuery.Imdb("B-Movie")) shouldBe Answer.Unknown
  }
}
