package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import services.facebook.{FacebookFetchRecorder, InMemoryFacebookFetches}
import tools.MutableClock

/** A film page served to Facebook's crawler is recorded under the URL the worker's re-scrapes ask
 *  about (`CityPath.filmUrl`, `FilmPageUrls`) — a page recorded under any other spelling would
 *  never be re-scraped. */
class FilmPageFacebookFetchSpec extends AnyFlatSpec with Matchers {

  private val Crawler = "facebookexternalhit/1.1 (+http://www.facebook.com/externalhit_uatext.php)"

  private def served(userAgent: String): Map[String, java.time.Instant] = {
    val fetches  = new InMemoryFacebookFetches
    val clock    = new MutableClock(java.time.Instant.parse("2026-10-14T18:00:00Z"))
    val recorder = new FacebookFetchRecorder(fetches, clock, _root_.tools.SpecStopwatch.Frozen)
    val ctrl = TestCityControllers.build(Seq(TestCityControllers.showing("Testowy Film", Some(2024), imdbId = Some("tt1"))),
      filmPageServed = recorder.served)
    // Facebook's crawler follows a shared link with its own query (`fbclid`): the page it holds is still the film's.
    val response = ctrl.movie.filmBySlug("poznan", "testowy-film")(
      EdgeRequest("/poznan/movie/testowy-film?fbclid=IwAR0abc").withHeaders(USER_AGENT -> userAgent))
    status(response) shouldBe OK
    recorder.stop()
    fetches.recorded
  }

  "A film page Facebook's crawler fetches" should "be recorded under the URL the worker re-scrapes" in {
    served(Crawler).keySet shouldBe Set("https://kinowo.net/poznan/movie/testowy-film")
  }

  "A film page a browser fetches" should "not be recorded" in {
    served("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 Version/18.0 Safari/605.1.15") shouldBe empty
  }
}
