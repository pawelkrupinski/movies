package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.OMDbClient
import tools.GetOnlyHttpFetch

class OMDbClientSpec extends AnyFlatSpec with Matchers {

  // Hand-written fixtures mirroring omdbapi.com: a `?t=` title search (carries
  // `imdbID`) and a `?i=…&tomatoes=true` lookup (carries `tomatoURL`). No live
  // HTTP in this suite.
  private def loadFixture(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"fixture not found: $path")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }
  private val TitleSearch = loadFixture("/fixtures/omdb/omdb_title_search.json")
  private val ById        = loadFixture("/fixtures/omdb/omdb_by_id.json")

  /** Routes `?t=` vs `?i=` requests to the two fixtures; records the last URL. */
  private class RoutingFetch extends GetOnlyHttpFetch {
    var lastUrl: String = ""
    def get(url: String): String = {
      lastUrl = url
      if (url.contains("?t=")) TitleSearch
      else if (url.contains("?i=")) ById
      else """{"Response":"False"}"""
    }
  }
  private def client(http: GetOnlyHttpFetch, key: Option[String] = Some("test-key")) =
    new OMDbClient(http, apiKey = key)

  // ── findImdbId (title+year search) ───────────────────────────────────────────

  "findImdbId" should "recover an imdb id when OMDb's returned title matches the query" in {
    // Query "Sirât" (â) vs OMDb "Sirat" — folded match passes the guard.
    client(new RoutingFetch).findImdbId(Seq("Sirât"), Some(2025)) shouldBe Some("tt32298285")
  }

  it should "reject a fuzzy hit whose returned title is unrelated to the query" in {
    val fetch = new GetOnlyHttpFetch {
      def get(url: String): String = """{"Title":"Some Other Film","imdbID":"tt9999999","Response":"True"}"""
    }
    client(fetch).findImdbId(Seq("Mawka"), Some(2026)) shouldBe None
  }

  it should "try the next title spelling when the first yields no match" in {
    var calls = 0
    val fetch = new GetOnlyHttpFetch {
      def get(url: String): String = {
        calls += 1
        if (url.contains("Mawka")) """{"Response":"False"}"""
        else """{"Title":"Mavka. The Forest Song","imdbID":"tt11808706","Response":"True"}"""
      }
    }
    client(fetch).findImdbId(Seq("Mawka", "Mavka. The Forest Song"), Some(2023)) shouldBe Some("tt11808706")
    calls shouldBe 2
  }

  it should "include the year in the query when supplied" in {
    val fetch = new RoutingFetch
    client(fetch).findImdbId(Seq("Sirat"), Some(2025))
    fetch.lastUrl should (include ("?t=Sirat") and include ("&y=2025"))
  }

  it should "return None and make NO HTTP call when the key is unset" in {
    val fetch = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("no HTTP when key unset") }
    client(fetch, key = None).findImdbId(Seq("Sirat"), Some(2025)) shouldBe None
  }

  it should "return None for all-blank titles without a call" in {
    val fetch = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("no call for blank titles") }
    client(fetch).findImdbId(Seq("", "   "), Some(2025)) shouldBe None
  }

  // ── rottenTomatoesUrl (by imdb id) ───────────────────────────────────────────

  "rottenTomatoesUrl" should "extract OMDb's tomatoURL" in {
    client(new RoutingFetch).rottenTomatoesUrl("tt5089534") shouldBe
      Some("https://www.rottentomatoes.com/m/freak_show")
  }

  it should "return None when tomatoURL is N/A" in {
    val fetch = new GetOnlyHttpFetch { def get(url: String): String = """{"tomatoURL":"N/A","Response":"True"}""" }
    client(fetch).rottenTomatoesUrl("tt0000001") shouldBe None
  }

  it should "hit the documented endpoint with the id, tomatoes flag and key" in {
    val fetch = new RoutingFetch
    client(fetch, key = Some("abc123")).rottenTomatoesUrl("tt5089534")
    fetch.lastUrl shouldBe "https://www.omdbapi.com/?i=tt5089534&tomatoes=true&apikey=abc123"
  }

  it should "return None and make NO HTTP call when the key is unset" in {
    val fetch = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("no HTTP when key unset") }
    client(fetch, key = None).rottenTomatoesUrl("tt5089534") shouldBe None
  }

  it should "swallow a network/HTTP failure and return None" in {
    val fetch = new GetOnlyHttpFetch { def get(url: String): String = throw new RuntimeException("HTTP 503") }
    client(fetch).rottenTomatoesUrl("tt5089534") shouldBe None
  }
}
