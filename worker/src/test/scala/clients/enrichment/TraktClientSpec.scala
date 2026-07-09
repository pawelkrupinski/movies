package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.TraktClient
import tools.HttpFetch

import scala.collection.mutable.ListBuffer

/**
 * Fixture-driven tests for TraktClient.
 *
 * Fixtures are REAL captures (every result carries an `ids` block with `imdb` +
 * `tmdb` together — that is the whole point of Trakt as an id bridge), trimmed
 * to the fields the parser reads:
 *   id lookup: GET https://api.trakt.tv/search/imdb/tt0111161?type=movie
 *   title:     GET https://api.trakt.tv/search/movie?query=Dune  (two exact-title
 *              "Dune" entries: 2021 tmdb 438631 + 1984 tmdb 841, unfiltered by year)
 * Trakt requires the `trakt-api-key` (client_id) header on every call, so the
 * client is feature-gated on TRAKT_API_CLIENT_ID and makes NO HTTP without it.
 */
class TraktClientSpec extends AnyFlatSpec with Matchers {

  private def loadFixture(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"fixture not found: $path")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }

  /** Stub whose `get` is a url→body function; records requested urls + headers. */
  private class RecordingFetch(f: String => String) extends HttpFetch {
    val urls    = ListBuffer.empty[String]
    val headers = ListBuffer.empty[Map[String, String]]
    def get(url: String): String = { urls += url; f(url) }
    override def get(url: String, hs: Map[String, String]): String = { headers += hs; get(url) }
    override def post(url: String, body: String, contentType: String): String =
      throw new RuntimeException("TraktClient should not POST")
  }

  private def client(f: String => String, key: Option[String] = Some("cid")) =
    new TraktClient(new RecordingFetch(f), apiKey = key)

  // ── findByImdbId: exact id bridge ────────────────────────────────────────────

  "findByImdbId" should "return the tmdb id (and imdb id) from Trakt's id lookup" in {
    val trakt = client(_ => loadFixture("/fixtures/trakt/search_imdb_tt0111161.json"))
    val m = trakt.findByImdbId("tt0111161")
    m.flatMap(_.tmdbId) shouldBe Some(278)
    m.flatMap(_.imdbId) shouldBe Some("tt0111161")
  }

  it should "hit /search/imdb/{id}?type=movie with the trakt-api-key and version headers" in {
    val fetch = new RecordingFetch(_ => loadFixture("/fixtures/trakt/search_imdb_tt0111161.json"))
    new TraktClient(fetch, apiKey = Some("abc123")).findByImdbId("tt0111161")
    fetch.urls.head shouldBe "https://api.trakt.tv/search/imdb/tt0111161?type=movie"
    fetch.headers.head should contain ("trakt-api-key" -> "abc123")
    fetch.headers.head should contain ("trakt-api-version" -> "2")
  }

  it should "return None and make NO HTTP call when the key is unset" in {
    client(_ => throw new RuntimeException("no HTTP when key unset"), key = None)
      .findByImdbId("tt0111161") shouldBe None
  }

  it should "return None and make NO HTTP call for a non-tt id" in {
    val fetch = new RecordingFetch(_ => fail("must not call HTTP for a non-imdb id"))
    new TraktClient(fetch, apiKey = Some("cid")).findByImdbId("278") shouldBe None
    fetch.urls shouldBe empty
  }

  it should "swallow a network failure and return None" in {
    client(_ => throw new RuntimeException("HTTP 503")).findByImdbId("tt0111161") shouldBe None
  }

  // ── search: title candidates each carrying both ids ──────────────────────────

  "search" should "return every candidate with its imdb + tmdb ids" in {
    val trakt = client(_ => loadFixture("/fixtures/trakt/search_movie_dune.json"))
    val hits  = trakt.search("Dune", None)
    hits.flatMap(_.tmdbId) shouldBe Seq(438631, 841)
    hits.map(_.year) shouldBe Seq(Some(2021), Some(1984))
  }

  it should "url-encode the query and scope by year" in {
    val fetch = new RecordingFetch(_ => "[]")
    new TraktClient(fetch, apiKey = Some("cid")).search("La La Land", Some(2016))
    fetch.urls.head shouldBe "https://api.trakt.tv/search/movie?query=La+La+Land&years=2016"
  }

  it should "return empty and make NO HTTP call when the key is unset" in {
    client(_ => throw new RuntimeException("no HTTP when key unset"), key = None)
      .search("Dune", Some(2021)) shouldBe empty
  }

  it should "swallow a network failure and return empty" in {
    client(_ => throw new RuntimeException("HTTP 503")).search("Dune", Some(2021)) shouldBe empty
  }
}
