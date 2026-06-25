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
