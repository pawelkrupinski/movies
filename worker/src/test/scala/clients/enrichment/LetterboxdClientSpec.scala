package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.LetterboxdClient
import services.enrichment.LetterboxdClient.FilmIds
import tools.GetOnlyHttpFetch

class LetterboxdClientSpec extends AnyFlatSpec with Matchers {

  // Fixture: test/resources/fixtures/letterboxd/film_inception.html — a real
  // /tmdb/27205/ page (redirects to /film/inception/) trimmed to the <body>
  // id attributes and the IMDb/TMDb footer links we parse.
  private def loadFixture(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"fixture not found: $path")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }
  private val InceptionFixture = "/fixtures/letterboxd/film_inception.html"

  private def stub(body: String) = new GetOnlyHttpFetch {
    def get(url: String): String = body
  }
  private def notFound = new GetOnlyHttpFetch {
    def get(url: String): String = throw new RuntimeException("HTTP 404")
  }

  "parse" should "extract both the TMDB id (body attr) and the IMDb id (footer link)" in {
    val c = new LetterboxdClient(stub(""))
    c.parse(loadFixture(InceptionFixture)) shouldBe FilmIds(Some(27205), Some("tt1375666"))
  }

  it should "suppress the tmdbId when data-tmdb-type is not a movie (Letterboxd also lists TV)" in {
    val c = new LetterboxdClient(stub(""))
    val tv =
      """<html><body data-tmdb-type="tv" data-tmdb-id="1396">
         |<a href="http://www.imdb.com/title/tt0903747/">IMDb</a></body></html>""".stripMargin
    c.parse(tv) shouldBe FilmIds(None, Some("tt0903747"))
  }

  it should "return empty ids for a page with neither marker" in {
    val c = new LetterboxdClient(stub(""))
    c.parse("<html><body><p>nothing here</p></body></html>") shouldBe FilmIds(None, None)
  }

  "byImdbId" should "fetch the /imdb/{id}/ page and read its ids" in {
    val fixture = loadFixture(InceptionFixture)
    val c = new LetterboxdClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url == "https://letterboxd.com/imdb/tt1375666/") fixture
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.byImdbId("tt1375666") shouldBe Some(FilmIds(Some(27205), Some("tt1375666")))
  }

  "byTmdbId" should "fetch the /tmdb/{id}/ page and read its ids" in {
    val fixture = loadFixture(InceptionFixture)
    val c = new LetterboxdClient(new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url == "https://letterboxd.com/tmdb/27205/") fixture
        else throw new RuntimeException(s"unexpected URL: $url")
    })
    c.byTmdbId(27205) shouldBe Some(FilmIds(Some(27205), Some("tt1375666")))
  }

  it should "return None when the film page 404s (unknown id)" in {
    val c = new LetterboxdClient(notFound)
    c.byImdbId("tt0000000") shouldBe None
    c.byTmdbId(999999999) shouldBe None
  }
}
