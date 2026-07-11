package services.enrichment

import models.{MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{ImdbIdMissing, InProcessEventBus}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import tools.HttpFetch
import tools.Eventually.eventually

/**
 * Tests for `ImdbIdResolver` — the class extracted out of `ImdbRatings` to
 * own the IMDb-id discovery path. Confirms that an `ImdbIdMissing` event
 * triggers a suggestion lookup and the id gets written back to the cache row
 * (from where the EnrichmentReaper picks up the now-eligible IMDb rating).
 * `ImdbRatings`'s rating-refresh behaviour is covered in its own spec; this
 * spec is intentionally narrow.
 */
class ImdbIdResolverSpec extends AnyFlatSpec with Matchers {

  // ── Test scaffolding ────────────────────────────────────────────────────────

  // Serves IMDb's suggestion endpoint (GET only). The `suggestions` map keys
  // are substrings the URL is expected to contain (case-insensitive); the
  // value is the JSON body to return. No POST path needed — the resolver
  // never calls into ratings.
  private def imdbStub(suggestions: Map[String, String]): ImdbClient =
    new ImdbClient(http = new HttpFetch {
      def get(url: String): String =
        suggestions.collectFirst { case (q, body) if url.toLowerCase.contains(q.toLowerCase) => body }
          .getOrElse(throw new RuntimeException(s"no stubbed suggestion for $url"))
      override def post(url: String, body: String, contentType: String): String =
        throw new RuntimeException("ImdbIdResolver should not POST")
    })

  private def loadFixture(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"fixture not found: $path")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }

  // ── onImdbIdMissing ─────────────────────────────────────────────────────────

  "onImdbIdMissing" should "find the IMDb id via the suggestion endpoint and write it back to the cache" in {
    val bus = new InProcessEventBus()
    val tmdbOnly = MovieRecord(
      tmdbId = Some(1024),
      data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Mortal Kombat II")))
    )
    val repository  = new InMemoryMovieRepository(Seq(("Mortal Kombat 2", Some(2026), tmdbOnly)))
    val cache = new CaffeineMovieCache(repository)
    val resolver = new ImdbIdResolver(cache, imdbStub(
      Map("suggestion" -> loadFixture("/fixtures/imdb/suggestion_mortal_kombat_ii.json"))
    ))
    bus.subscribe(resolver.onImdbIdMissing)

    bus.publish(ImdbIdMissing("Mortal Kombat 2", Some(2026), "Mortal Kombat II"))

    eventually {
      cache.get(cache.keyOf("Mortal Kombat 2", Some(2026))).flatMap(_.imdbId) shouldBe Some("tt17490712")
    }
  }

  it should "no-op when the suggestion endpoint returns nothing usable" in {
    val bus = new InProcessEventBus()
    val tmdbOnly = MovieRecord(
      tmdbId = Some(1),
      data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Imaginary Film")))
    )
    val repository  = new InMemoryMovieRepository(Seq(("Imaginary Film", None, tmdbOnly)))
    val cache = new CaffeineMovieCache(repository)
    repository.upserts.clear()
    val resolver = new ImdbIdResolver(cache, imdbStub(Map("suggestion" -> """{"d":[]}""")))
    bus.subscribe(resolver.onImdbIdMissing)

    noException should be thrownBy bus.publish(ImdbIdMissing("Imaginary Film", None, "Imaginary Film"))
    Thread.sleep(100)
    repository.upserts shouldBe empty
  }

  it should "be a no-op when the row already has an imdbId (stale event raced with another resolver)" in {
    val bus = new InProcessEventBus()
    val resolved = MovieRecord(
      imdbId = Some("tt9999"), imdbRating = Some(8.0),
      tmdbId = Some(1),
      data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Foo")))
    )
    val repository  = new InMemoryMovieRepository(Seq(("Foo", None, resolved)))
    val cache = new CaffeineMovieCache(repository)
    repository.upserts.clear()
    // Stub that THROWS if findId or any HTTP call lands — confirms the
    // resolver short-circuits before hitting IMDb when the id is already
    // present on the row.
    val resolver = new ImdbIdResolver(cache, new ImdbClient(http = new HttpFetch {
      def get(url: String): String = throw new RuntimeException("findId should not be called")
      override def post(url: String, body: String, contentType: String): String =
        throw new RuntimeException("lookup should not be called")
    }))
    bus.subscribe(resolver.onImdbIdMissing)

    noException should be thrownBy bus.publish(ImdbIdMissing("Foo", None, "Foo"))
    Thread.sleep(100)
    repository.upserts shouldBe empty
  }

  // ── Trakt / Letterboxd id-crosswalk backstops ───────────────────────────────

  // GET-only stub for the Trakt / Letterboxd clients: matches on a URL substring.
  private class StubGet(routes: Seq[(String, String)]) extends tools.GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
  }

  "the Trakt backstop" should "recover the imdbId from a corroborated Trakt title search when IMDb abstains" in {
    val tmdbOnly = MovieRecord(
      tmdbId = Some(4242),
      data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Obscure Arthouse Film"), releaseYear = Some(2016)))
    )
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Obscure Arthouse Film", Some(2016), tmdbOnly))))
    val trakt = new TraktIdResolver(new TraktClient(
      new StubGet(Seq("/search/movie" ->
        """[{"type":"movie","movie":{"title":"Obscure Arthouse Film","year":2016,"ids":{"trakt":1,"tmdb":4242,"imdb":"tt7001001"}}}]""")),
      apiKey = Some("stub")))
    val resolver = new ImdbIdResolver(cache, imdbStub(Map("suggestion" -> """{"d":[]}""")),
      traktIdResolver = Some(trakt))

    resolver.resolveSync("Obscure Arthouse Film", Some(2016), "Obscure Arthouse Film")
    cache.get(cache.keyOf("Obscure Arthouse Film", Some(2016))).flatMap(_.imdbId) shouldBe Some("tt7001001")
  }

  "the Letterboxd backstop" should "recover the imdbId from the film's Letterboxd page when IMDb and Trakt abstain" in {
    val tmdbOnly = MovieRecord(
      tmdbId = Some(5252),
      data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Another Obscure Film"), releaseYear = Some(2017)))
    )
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Another Obscure Film", Some(2017), tmdbOnly))))
    val letterboxd = new LetterboxdIdResolver(new LetterboxdClient(
      new StubGet(Seq("/tmdb/5252/" ->
        """<html><body data-tmdb-id="5252" data-tmdb-type="movie"><a href="https://www.imdb.com/title/tt7002002/">imdb</a></body></html>"""))))
    val resolver = new ImdbIdResolver(cache, imdbStub(Map("suggestion" -> """{"d":[]}""")),
      letterboxdIdResolver = Some(letterboxd))

    resolver.resolveSync("Another Obscure Film", Some(2017), "Another Obscure Film")
    cache.get(cache.keyOf("Another Obscure Film", Some(2017))).flatMap(_.imdbId) shouldBe Some("tt7002002")
  }

  "the OMDb backstop" should "recover the imdbId via OMDb for a TMDB-less film when the whole IMDb ladder abstains" in {
    // A tmdbNoMatch film (no tmdbId → Letterboxd skipped) that IMDb's suggestion
    // endpoint doesn't index — the Malayalam/Indian long tail OMDb's English DB
    // covers. This is the rung that would previously only fire on the daily sweep.
    val noTmdb = MovieRecord(tmdbNoMatch = true)
    val cache  = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Varavu", Some(2026), noTmdb))))
    val omdb   = new OMDbClient(new StubGet(Seq("?t=" ->
      """{"Title":"Varavu","Year":"2026","imdbID":"tt37963237","Director":"Shaji Kailas","Response":"True"}""")),
      apiKey = Some("stub"))
    val resolver = new ImdbIdResolver(cache, imdbStub(Map("suggestion" -> """{"d":[]}""")),
      omdb = Some(omdb))

    resolver.resolveSync("Varavu", Some(2026), "Varavu")
    cache.get(cache.keyOf("Varavu", Some(2026))).flatMap(_.imdbId) shouldBe Some("tt37963237")
  }

  "the Cinemeta backstop" should "recover the imdbId via Cinemeta when every earlier rung (incl. OMDb) abstains" in {
    val noTmdb = MovieRecord(tmdbNoMatch = true)
    val cache  = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Cactus Pears", Some(2026), noTmdb))))
    val cinemeta = new CinemetaClient(new StubGet(Seq("search=" ->
      """{"metas":[{"id":"tt31000001","type":"movie","name":"Cactus Pears","releaseInfo":"2026"}]}""")))
    val resolver = new ImdbIdResolver(cache, imdbStub(Map("suggestion" -> """{"d":[]}""")),
      cinemeta = Some(cinemeta))

    resolver.resolveSync("Cactus Pears", Some(2026), "Cactus Pears")
    cache.get(cache.keyOf("Cactus Pears", Some(2026))).flatMap(_.imdbId) shouldBe Some("tt31000001")
  }

  // ── hint-keyed cache ─────────────────────────────────────────────────────────

  private def countingImdb(calls: java.util.concurrent.atomic.AtomicInteger): ImdbClient =
    new ImdbClient(http = new HttpFetch {
      def get(url: String): String = {
        calls.incrementAndGet()
        loadFixture("/fixtures/imdb/suggestion_mortal_kombat_ii.json")
      }
      override def post(url: String, body: String, contentType: String): String =
        throw new RuntimeException("ImdbIdResolver should not POST")
    })

  "the IMDb id cache" should "look up the same search once for two findIdFor calls" in {
    val calls = new java.util.concurrent.atomic.AtomicInteger(0)
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val resolver = new ImdbIdResolver(cache, countingImdb(calls),
      imdbIdCache = new services.resolution.WriteThroughResolutionCache(new services.resolution.InMemoryResolutionStore()))

    resolver.findIdFor("Mortal Kombat II", Some(2026)) shouldBe Some("tt17490712")
    resolver.findIdFor("Mortal Kombat II", Some(2026)) shouldBe Some("tt17490712")
    calls.get() shouldBe 1
  }

  it should "look up on every call without a cache (control)" in {
    val calls = new java.util.concurrent.atomic.AtomicInteger(0)
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val resolver = new ImdbIdResolver(cache, countingImdb(calls),
      imdbIdCache = services.resolution.ResolutionCache.passthrough)

    resolver.findIdFor("Mortal Kombat II", Some(2026))
    resolver.findIdFor("Mortal Kombat II", Some(2026))
    calls.get() shouldBe 2
  }

  // ── Wikidata fallback ────────────────────────────────────────────────────────

  it should "use the Wikidata fallback when both IMDb suggestion and director paths return nothing" in {
    val bus = new InProcessEventBus()
    // A film with a real Filmweb entity URL but no IMDb match from the suggestion endpoint
    val record = MovieRecord(
      tmdbId     = Some(1024),
      filmwebUrl = Some("https://www.filmweb.pl/film/Popiol+i+diament-1958-1118"),
      data       = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Ashes and Diamonds")))
    )
    val repository = new InMemoryMovieRepository(Seq(("Popiół i diament", Some(1958), record)))
    val cache      = new CaffeineMovieCache(repository)
    // IMDb suggestion returns no match; Wikidata stub returns tt0052080 for filmweb id 1118
    val wikidataStub = new WikidataClient(new HttpFetch {
      def get(url: String): String =
        if (url.contains("haswbstatement"))
          """{"query":{"search":[{"title":"Q722281"}]}}"""
        else if (url.contains("wbgetentities"))
          """{"entities":{"Q722281":{"claims":{"P345":[{"mainsnak":{"datavalue":{"value":"tt0052080"}}}]}}}}"""
        else throw new RuntimeException(s"unexpected url: $url")
      override def get(url: String, headers: Map[String, String]): String = get(url)
      override def post(url: String, body: String, contentType: String): String = ???
    })
    val resolver = new ImdbIdResolver(cache, imdbStub(Map("suggestion" -> """{"d":[]}""")),
      wikidata = Some(wikidataStub))
    bus.subscribe(resolver.onImdbIdMissing)

    bus.publish(ImdbIdMissing("Popiół i diament", Some(1958), "Ashes and Diamonds"))

    eventually {
      cache.get(cache.keyOf("Popiół i diament", Some(1958))).flatMap(_.imdbId) shouldBe Some("tt0052080")
    }
  }

  it should "backfill the RT and Metacritic page URLs from the Wikidata harvest" in {
    val bus = new InProcessEventBus()
    // A film with no imdbId AND no RT/MC page URL yet — one Wikidata claims call
    // recovers the imdbId and the RT/MC slugs (which those rating clients would
    // otherwise slug-probe for).
    val record = MovieRecord(
      tmdbId     = Some(603),
      filmwebUrl = Some("https://www.filmweb.pl/film/Matrix-1999-33986"),
      data       = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("The Matrix")))
    )
    val repository = new InMemoryMovieRepository(Seq(("Matrix", Some(1999), record)))
    val cache      = new CaffeineMovieCache(repository)
    val wikidataStub = new WikidataClient(new HttpFetch {
      def get(url: String): String =
        if (url.contains("haswbstatement")) """{"query":{"search":[{"title":"Q83495"}]}}"""
        else if (url.contains("wbgetentities"))
          """{"entities":{"Q83495":{"claims":{
            |"P345":[{"mainsnak":{"datavalue":{"value":"tt0133093"}}}],
            |"P1258":[{"mainsnak":{"datavalue":{"value":"m/the_matrix"}}}],
            |"P1712":[{"mainsnak":{"datavalue":{"value":"movie/the-matrix"}}}]
            |}}}}""".stripMargin
        else throw new RuntimeException(s"unexpected url: $url")
      override def get(url: String, headers: Map[String, String]): String = get(url)
      override def post(url: String, body: String, contentType: String): String = ???
    })
    val resolver = new ImdbIdResolver(cache, imdbStub(Map("suggestion" -> """{"d":[]}""")),
      wikidata = Some(wikidataStub))
    bus.subscribe(resolver.onImdbIdMissing)

    bus.publish(ImdbIdMissing("Matrix", Some(1999), "The Matrix"))

    eventually {
      val row = cache.get(cache.keyOf("Matrix", Some(1999)))
      row.flatMap(_.imdbId)            shouldBe Some("tt0133093")
      row.flatMap(_.rottenTomatoesUrl) shouldBe Some("https://www.rottentomatoes.com/m/the_matrix")
      row.flatMap(_.metacriticUrl)     shouldBe Some("https://www.metacritic.com/movie/the-matrix")
    }
  }

  it should "skip the Wikidata fallback when filmwebUrl is a search redirect (no entity id)" in {
    val bus = new InProcessEventBus()
    val record = MovieRecord(
      tmdbId     = Some(1025),
      filmwebUrl = Some("https://www.filmweb.pl/search?query=Unknown+Film"),
      data       = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Unknown Film")))
    )
    val repository = new InMemoryMovieRepository(Seq(("Unknown Film", Some(2024), record)))
    val cache      = new CaffeineMovieCache(repository)
    repository.upserts.clear()
    val wikidataThrows = new WikidataClient(new HttpFetch {
      def get(url: String): String = throw new RuntimeException("Wikidata should not be called for search URLs")
      override def get(url: String, headers: Map[String, String]): String = get(url)
      override def post(url: String, body: String, contentType: String): String = ???
    })
    val resolver = new ImdbIdResolver(cache, imdbStub(Map("suggestion" -> """{"d":[]}""")),
      wikidata = Some(wikidataThrows))
    bus.subscribe(resolver.onImdbIdMissing)

    noException should be thrownBy bus.publish(ImdbIdMissing("Unknown Film", Some(2024), "Unknown Film"))
    Thread.sleep(100)
    repository.upserts shouldBe empty
  }

  // ── Director-based disambiguation ───────────────────────────────────────────

  "onImdbIdMissing" should "use director data from the cache record when the film is listed under a different title on IMDb" in {
    // The film is in our cache as "Nasz Film" but IMDb lists it under "IMDb Title"
    // (AKA / foreign-title situation). parseSuggestions finds no title match → None.
    // The director fallback reads the director from the cache record and confirms
    // the match via a details POST.
    val bus = new InProcessEventBus()
    // director is derived from sourceData — pass via data map
    val tmdbOnly = MovieRecord(
      tmdbId = Some(1024),
      data   = Map[Source, SourceData](Tmdb -> SourceData(
        originalTitle = Some("Nasz Film"),
        director      = Seq("Jakub Pączek")
      ))
    )
    val repository = new InMemoryMovieRepository(Seq(("Nasz Film", Some(2026), tmdbOnly)))
    val cache      = new CaffeineMovieCache(repository)
    // IMDb suggestion returns the film under a different title
    val suggestionBody =
      """{"d":[{"id":"tt9999999","l":"IMDb Title","q":"feature","qid":"movie","rank":1}]}"""
    // IMDb stores director without diacritics; record has the Polish form — deburr match
    val detailsBody =
      """{"data":{"title":{"titleText":{"text":"IMDb Title"},"originalTitleText":{"text":"IMDb Title"},
        |"releaseYear":null,"runtime":null,"ratingsSummary":{"aggregateRating":0,"voteCount":0},
        |"countriesOfOrigin":{"countries":[]},"primaryImage":null,
        |"principalCredits":[{"category":{"id":"director"},"credits":[
        |  {"name":{"nameText":{"text":"Jakub Paczek"}}}
        |]}]}}}""".stripMargin
    val resolver = new ImdbIdResolver(cache, new ImdbClient(http = new HttpFetch {
      def get(url: String): String = suggestionBody
      override def post(url: String, body: String, contentType: String): String = detailsBody
    }))
    bus.subscribe(resolver.onImdbIdMissing)
    bus.publish(ImdbIdMissing("Nasz Film", Some(2026), "Nasz Film"))
    eventually {
      cache.get(cache.keyOf("Nasz Film", Some(2026))).flatMap(_.imdbId) shouldBe Some("tt9999999")
    }
  }
}
