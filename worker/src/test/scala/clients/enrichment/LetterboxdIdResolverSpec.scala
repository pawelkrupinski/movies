package clients.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.{LetterboxdClient, LetterboxdIdResolver}
import tools.GetOnlyHttpFetch

class LetterboxdIdResolverSpec extends AnyFlatSpec with Matchers {

  private def loadFixture(path: String): String = {
    val stream = getClass.getResourceAsStream(path)
    require(stream != null, s"fixture not found: $path")
    try scala.io.Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()
  }
  private val InceptionFixture = "/fixtures/letterboxd/film_inception.html"

  private def clientReturning(pageFor: String => String) =
    new LetterboxdClient(new GetOnlyHttpFetch {
      def get(url: String): String = pageFor(url)
    })

  "resolveTmdbId" should "recover the tmdbId from a known imdbId" in {
    val fixture = loadFixture(InceptionFixture)
    val resolver = new LetterboxdIdResolver(clientReturning(_ => fixture))
    resolver.resolveTmdbId("tt1375666") shouldBe Some(27205)
  }

  "resolveImdbId" should "recover the imdbId from a known tmdbId" in {
    val fixture = loadFixture(InceptionFixture)
    val resolver = new LetterboxdIdResolver(clientReturning(_ => fixture))
    resolver.resolveImdbId(27205) shouldBe Some("tt1375666")
  }

  // Guard: a Letterboxd redirect that lands on the WRONG film must not yield an
  // id. If we query /imdb/tt9999999/ but the page echoes tt1375666, the ids
  // don't correspond to our query — reject rather than persist a mismatch.
  it should "return None when the page's echoed id does not match the queried id" in {
    val fixture = loadFixture(InceptionFixture) // echoes tt1375666 / 27205
    val resolver = new LetterboxdIdResolver(clientReturning(_ => fixture))
    resolver.resolveTmdbId("tt9999999") shouldBe None
    resolver.resolveImdbId(11111111) shouldBe None
  }

  "resolveTmdbId" should "return None when the film page is unknown (404)" in {
    val resolver = new LetterboxdIdResolver(new LetterboxdClient(new GetOnlyHttpFetch {
      def get(url: String): String = throw new RuntimeException("HTTP 404")
    }))
    resolver.resolveTmdbId("tt0000000") shouldBe None
  }
}
