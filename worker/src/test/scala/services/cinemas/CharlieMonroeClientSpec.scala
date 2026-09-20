package services.cinemas

import org.scalatest.matchers.should.Matchers
import clients.tools.{FakeHttpFetch, FixtureFile}
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.CharlieMonroeClient

/** Exercises CharlieMonroeClient's detail-page parsing against the recorded
  * kinomalta.pl WPMovieLibrary fixture (movies/prosta-historia). */
class CharlieMonroeClientSpec extends AnyFlatSpec with Matchers {

  private def fixture(name: String): String =
    FixtureFile.read(s"test/resources/fixtures/08-06-2026/kinomalta.pl/$name")

  // The fetch is unused here — parseDirector/parseCountries are pure
  // String => Seq[String] functions fed the fixture HTML directly.
  private val client = new CharlieMonroeClient(new FakeHttpFetch("charlie-monroe"))

  "parseDirector" should "extract the director from the Reżyseria meta block" in {
    client.parseDirector(fixture("movies/prosta-historia")).should(contain("David Lynch"))
  }

  "parseCountries" should "still extract the production country" in {
    client.parseCountries(fixture("movies/prosta-historia")).should(not(be(empty)))
  }
}
