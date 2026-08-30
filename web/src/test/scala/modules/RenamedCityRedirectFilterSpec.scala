package modules

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.{RequestHeader, Result, Results}
import play.api.test.FakeRequest

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

/**
 * A renamed city keeps answering at the slug it published.
 *
 * `san-francisco` became `san-francisco-bay-area` while both were live: the old
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
  private def run(path: String, mountPath: String = "/us/"): Result = {
    val next: RequestHeader => Future[Result] = rh => Future.successful(Results.Ok(rh.path))
    val filter = new RenamedCityRedirectFilter(mountPath)
    Await.result(filter(next)(FakeRequest("GET", path)), 5.seconds)
  }

  "A path under a renamed city" should "301 to the same path under the current slug" in {
    val res = run("/us/san-francisco/")
    res.header.status shouldBe 301
    res.header.headers("Location") shouldBe "/us/san-francisco-bay-area/"
  }

  it should "carry the rest of the path, so a deep link lands on the page it named" in {
    run("/us/san-francisco/movie/dune").header.headers("Location") shouldBe
      "/us/san-francisco-bay-area/movie/dune"
    run("/us/san-francisco/api/repertoire").header.headers("Location") shouldBe
      "/us/san-francisco-bay-area/api/repertoire"
  }

  it should "keep the query string, which is where the filters live" in {
    run("/us/san-francisco/movies?director=Nolan").header.headers("Location") shouldBe
      "/us/san-francisco-bay-area/movies?director=Nolan"
  }

  "Every other path" should "pass through untouched" in {
    run("/us/los-angeles/").header.status shouldBe 200
    run("/us/").header.status shouldBe 200
    run("/us/assets/img/og-home-us.jpg").header.status shouldBe 200
  }

  it should "not rewrite a film whose own slug matches a renamed city" in {
    // Only the CITY segment is looked up, so a film called "San Francisco" in
    // some other metro is left alone.
    val res = run("/us/new-york/movie/san-francisco")
    res.header.status shouldBe 200
    res.header.headers.get("Location") shouldBe None
  }

  "A deployment served at the root" should "redirect without a mount prefix" in {
    // Poland is at `/`, so the city is the first segment of the path itself.
    val res = run("/san-francisco/", mountPath = "/")
    res.header.status shouldBe 301
    res.header.headers("Location") shouldBe "/san-francisco-bay-area/"
  }
}
