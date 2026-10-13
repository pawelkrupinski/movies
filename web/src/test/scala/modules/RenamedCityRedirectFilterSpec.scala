package modules

import tools.SpecTimeouts

import models.Country
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.{RequestHeader, Result, Results}
import play.api.test.FakeRequest

import scala.concurrent.{Await, Future}

/**
 * A renamed city keeps answering at the slug it published.
 *
 * `san-francisco-bay-area` became `san-francisco` (the city alone, 2026-10) while both were live: the old
 * slug is in the sitemap, in the `city` cookie of everyone who has visited, and
 * in whatever a search engine indexed. Every one of those has to land on the
 * page rather than on `withCity`'s 404.
 */
class RenamedCityRedirectFilterSpec extends AnyFlatSpec with Matchers {

  private given system: ActorSystem  = ActorSystem("renamed-city-redirect-spec")
  private given mat: Materializer    = Materializer(system)

  /** The filter over a `next` that stands in for the router — it answers 200
   *  with the path it was asked for, so a test can tell "passed through" from
   *  "redirected" and see what the router would have received. */
  private def run(path: String, country: Country = Country.UnitedStates): Result = {
    val next: RequestHeader => Future[Result] = rh => Future.successful(Results.Ok(rh.path))
    val filter = new RenamedCityRedirectFilter(country, services.metrics.SpecLegacyAddressMetrics.fresh())
    Await.result(filter(next)(FakeRequest("GET", path)), SpecTimeouts.Io)
  }

  "A path under a renamed city" should "301 to the same path under the current slug" in {
    val res = run("/us/san-francisco-bay-area/")
    res.header.status shouldBe 301
    res.header.headers("Location") shouldBe "/us/san-francisco/"
  }

  it should "carry the rest of the path, so a deep link lands on the page it named" in {
    run("/us/san-francisco-bay-area/movie/dune").header.headers("Location") shouldBe
      "/us/san-francisco/movie/dune"
    run("/us/san-francisco-bay-area/api/repertoire").header.headers("Location") shouldBe
      "/us/san-francisco/api/repertoire"
  }

  it should "keep the query string, which is where the filters live" in {
    run("/us/san-francisco-bay-area/movies?director=Nolan").header.headers("Location") shouldBe
      "/us/san-francisco/movies?director=Nolan"
  }

  "Every other path" should "pass through untouched" in {
    run("/us/los-angeles/").header.status shouldBe 200
    run("/us/").header.status shouldBe 200
    run("/us/assets/img/og-home-us.jpg").header.status shouldBe 200
  }

  it should "not rewrite a film whose own slug matches a renamed city" in {
    // Only the CITY segment is looked up, so a film called "San Francisco Bay Area" in
    // some other metro is left alone.
    val res = run("/us/new-york/movie/san-francisco-bay-area")
    res.header.status shouldBe 200
    res.header.headers.get("Location") shouldBe None
  }

  "A deployment served at the root" should "redirect without a mount prefix" in {
    // Poland is at `/`, so the city is the first segment of the path itself.
    val res = run("/ketrzyn/", Country.Poland)
    res.header.status shouldBe 301
    res.header.headers("Location") shouldBe "/gizycko/"
  }

  "A slug" should "only ever mean its own country's place" in {
    // `san-francisco-bay-area` is a retired US slug: on the UK's deployment it is
    // nothing at all, so the router (here, `next`) answers it.
    run("/uk/san-francisco-bay-area/", Country.UnitedKingdom).header.status shouldBe 200
    // `york` is a live UK page and a live US page; neither redirects.
    run("/uk/york/", Country.UnitedKingdom).header.status shouldBe 200
    run("/us/york/").header.status shouldBe 200
  }

  "A town a page covers, or a page's qualified slug," should "301 to the page" in {
    // Ilkley is on a cluster page; the UK's York page is `/uk/york/`, never
    // `/uk/york-north-yorkshire/`.
    val ilkley = Country.UnitedKingdom.slugAliases("ilkley")
    run("/uk/ilkley/", Country.UnitedKingdom).header.headers("Location") shouldBe s"/uk/$ilkley/"
    run("/uk/york-north-yorkshire/movie/dune", Country.UnitedKingdom).header.headers("Location") shouldBe
      "/uk/york/movie/dune"
    // A formerly qualified US page's URL, published while the UK held its name.
    run("/us/birmingham-alabama/").header.headers("Location") shouldBe "/us/birmingham/"
  }

  "A redirected slug with no trailing slash" should "land on the listing's slash form in one hop" in {
    run("/us/san-francisco-bay-area").header.headers("Location") shouldBe "/us/san-francisco/"
  }
}
