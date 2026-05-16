package services.enrichment

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{EventBus, ImdbIdMissing, ImdbIdResolved}
import services.movies.{InMemoryMovieRepo, MovieCache}
import tools.HttpFetch
import tools.Eventually.eventually

import scala.collection.mutable

/**
 * Tests for `ImdbIdResolver` — the class extracted out of `ImdbRatings` to
 * own the IMDb-id discovery path. Confirms that an `ImdbIdMissing` event
 * triggers a suggestion lookup, the id gets written to the cache row, and
 * an `ImdbIdResolved` event fires for downstream consumers (rating
 * fetchers). `ImdbRatings`'s rating-refresh behaviour is covered in its
 * own spec; this spec is intentionally narrow.
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

  "onImdbIdMissing" should "find the IMDb id via the suggestion endpoint, write it back, and publish ImdbIdResolved" in {
    val bus = new EventBus()
    val tmdbOnly = MovieRecord(
      imdbId        = None,
      imdbRating    = None,
      metascore     = None,
      originalTitle = Some("Mortal Kombat II"),
      tmdbId        = Some(1024)
    )
    val repo  = new InMemoryMovieRepo(Seq(("Mortal Kombat 2", Some(2026), tmdbOnly)))
    val cache = new MovieCache(repo)
    val resolver = new ImdbIdResolver(cache, imdbStub(
      Map("suggestion" -> loadFixture("/fixtures/imdb/suggestion_mortal_kombat_ii.json"))
    ), bus)

    val resolved = mutable.ListBuffer.empty[ImdbIdResolved]
    bus.subscribe { case e: ImdbIdResolved => resolved.append(e) }
    bus.subscribe(resolver.onImdbIdMissing)

    bus.publish(ImdbIdMissing("Mortal Kombat 2", Some(2026), "Mortal Kombat II"))

    eventually {
      val row = cache.get(cache.keyOf("Mortal Kombat 2", Some(2026))).get
      row.imdbId shouldBe Some("tt17490712")
      resolved.toList shouldBe List(ImdbIdResolved("Mortal Kombat 2", Some(2026), "tt17490712"))
    }
  }

  it should "no-op when the suggestion endpoint returns nothing usable" in {
    val bus = new EventBus()
    val tmdbOnly = MovieRecord(imdbId = None, imdbRating = None, metascore = None,
                               originalTitle = Some("Imaginary Film"), tmdbId = Some(1))
    val repo  = new InMemoryMovieRepo(Seq(("Imaginary Film", None, tmdbOnly)))
    val cache = new MovieCache(repo)
    repo.upserts.clear()
    val resolver = new ImdbIdResolver(cache, imdbStub(Map("suggestion" -> """{"d":[]}""")), bus)

    val resolved = mutable.ListBuffer.empty[ImdbIdResolved]
    bus.subscribe { case e: ImdbIdResolved => resolved.append(e) }
    bus.subscribe(resolver.onImdbIdMissing)

    noException should be thrownBy bus.publish(ImdbIdMissing("Imaginary Film", None, "Imaginary Film"))
    Thread.sleep(100)
    repo.upserts shouldBe empty
    resolved     shouldBe empty
  }

  it should "be a no-op when the row already has an imdbId (stale event raced with another resolver)" in {
    val bus = new EventBus()
    val resolved = MovieRecord(imdbId = Some("tt9999"), imdbRating = Some(8.0), metascore = None,
                               originalTitle = Some("Foo"), tmdbId = Some(1))
    val repo  = new InMemoryMovieRepo(Seq(("Foo", None, resolved)))
    val cache = new MovieCache(repo)
    repo.upserts.clear()
    // Stub that THROWS if findId or any HTTP call lands — confirms the
    // resolver short-circuits before hitting IMDb when the id is already
    // present on the row.
    val resolver = new ImdbIdResolver(cache, new ImdbClient(http = new HttpFetch {
      def get(url: String): String = throw new RuntimeException("findId should not be called")
      override def post(url: String, body: String, contentType: String): String =
        throw new RuntimeException("lookup should not be called")
    }), bus)

    val emitted = mutable.ListBuffer.empty[ImdbIdResolved]
    bus.subscribe { case e: ImdbIdResolved => emitted.append(e) }
    bus.subscribe(resolver.onImdbIdMissing)

    noException should be thrownBy bus.publish(ImdbIdMissing("Foo", None, "Foo"))
    Thread.sleep(100)
    repo.upserts shouldBe empty
    emitted      shouldBe empty
  }
}
