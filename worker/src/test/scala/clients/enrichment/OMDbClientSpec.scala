package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.{OMDbClient, OmdbRatings}
import tools.GetOnlyHttpFetch

class OMDbClientSpec extends AnyFlatSpec with Matchers {

  // Hand-written fixtures captured to mirror the omdbapi.com `?i=<id>&tomatoes=true`
  // response shape (full ratings vs everything N/A). No live HTTP in this suite.
  private def loadFixture(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"fixture not found: $path")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }

  private val FullFixture = "/fixtures/omdb/omdb_full.json"
  private val NaFixture   = "/fixtures/omdb/omdb_na.json"

  // Key present so `ratings` would make a call; the stub returns the fixture.
  private def clientReturning(body: String): OMDbClient =
    new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = body },
      apiKey = Some("test-key")
    )

  // ── parse ──────────────────────────────────────────────────────────────────

  "parse" should "extract imdbRating, Rotten Tomatoes % and Metacritic /100" in {
    val r = clientReturning("").parse(loadFixture(FullFixture))
    r shouldBe OmdbRatings(imdbRating = Some(7.5), rottenTomatoes = Some(85), metascore = Some(72))
  }

  it should "map every N/A field to None" in {
    val r = clientReturning("").parse(loadFixture(NaFixture))
    r shouldBe OmdbRatings(None, None, None)
  }

  it should "parse a percentage Rotten Tomatoes value" in {
    val body = """{"Ratings":[{"Source":"Rotten Tomatoes","Value":"42%"}]}"""
    clientReturning("").parse(body).rottenTomatoes shouldBe Some(42)
  }

  it should "parse the /100 Metacritic value from the Ratings array when top-level Metascore is N/A" in {
    val body =
      """{"Metascore":"N/A","Ratings":[{"Source":"Metacritic","Value":"61/100"}]}"""
    clientReturning("").parse(body).metascore shouldBe Some(61)
  }

  it should "prefer the top-level Metascore over the Ratings-array Metacritic entry" in {
    val body =
      """{"Metascore":"90","Ratings":[{"Source":"Metacritic","Value":"61/100"}]}"""
    clientReturning("").parse(body).metascore shouldBe Some(90)
  }

  it should "return an all-empty result for a malformed / non-JSON body" in {
    clientReturning("").parse("not json at all") shouldBe OmdbRatings(None, None, None)
    clientReturning("").parse("{}")             shouldBe OmdbRatings(None, None, None)
  }

  // ── feature gate ─────────────────────────────────────────────────────────────

  "ratings" should "return None and make NO HTTP call when OMDB_API_KEY is unset" in {
    val client = new OMDbClient(
      http = new GetOnlyHttpFetch {
        def get(url: String): String = throw new RuntimeException("no HTTP call expected when key unset")
      },
      apiKey = None
    )
    client.ratings("tt0133093") shouldBe None
  }

  it should "fetch and parse when the key is present" in {
    clientReturning(loadFixture(FullFixture)).ratings("tt0133093") shouldBe
      Some(OmdbRatings(Some(7.5), Some(85), Some(72)))
  }

  it should "hit the documented omdbapi.com endpoint with the id, tomatoes flag and key" in {
    var requested = ""
    val client = new OMDbClient(
      http = new GetOnlyHttpFetch {
        def get(url: String): String = { requested = url; """{"imdbRating":"7.5"}""" }
      },
      apiKey = Some("abc123")
    )
    client.ratings("tt0133093")
    requested shouldBe "https://www.omdbapi.com/?i=tt0133093&tomatoes=true&apikey=abc123"
  }

  it should "drop an all-N/A response (nothing usable to backfill)" in {
    clientReturning(loadFixture(NaFixture)).ratings("tt9999999") shouldBe None
  }

  it should "return None for a blank imdb id without making a call" in {
    val client = new OMDbClient(
      http = new GetOnlyHttpFetch {
        def get(url: String): String = throw new RuntimeException("no call for blank id")
      },
      apiKey = Some("test-key")
    )
    client.ratings("   ") shouldBe None
  }

  it should "swallow a network/HTTP failure and return None" in {
    val client = new OMDbClient(
      http = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("HTTP 503") },
      apiKey = Some("test-key")
    )
    client.ratings("tt0133093") shouldBe None
  }
}
