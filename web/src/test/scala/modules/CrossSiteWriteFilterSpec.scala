package modules

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.Results
import play.api.test.FakeRequest

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/** Every state-changing route is `nocsrf`, so without this filter the ONLY thing
 *  keeping another site from driving them with a visitor's session cookie is the
 *  cookie's SameSite attribute. The filter is the second, attribute-independent
 *  layer: a browser-issued write that the browser itself labels cross-site is
 *  refused before it reaches a controller. */
class CrossSiteWriteFilterSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private implicit val sys: ActorSystem  = ActorSystem("cross-site-write-filter-spec")
  private implicit val mat: Materializer = Materializer(sys)

  override def afterAll(): Unit = sys.terminate()

  private val filter = new CrossSiteWriteFilter()

  private def status(method: String, path: String, headers: (String, String)*): Int =
    Await.result(
      filter.apply(_ => Future.successful(Results.Ok("reached the controller")))(
        FakeRequest(method, path).withHeaders(headers*)),
      2.seconds).header.status

  private def rejected(result: Int): Boolean = result == 403

  "CrossSiteWriteFilter" should "refuse a cross-site browser DELETE of the account" in {
    rejected(status("DELETE", "/api/me", "Sec-Fetch-Site" -> "cross-site", "Cookie" -> "PLAY_SESSION=x")) shouldBe true
  }

  it should "refuse every unsafe method a cross-site page can issue" in {
    Seq("POST" -> "/auth/sessions/revoke", "PUT" -> "/api/me/state", "PATCH" -> "/api/me/state",
        "POST" -> "/admin/config/set", "POST" -> "/tasks/run/x").foreach { case (method, path) =>
      withClue(s"$method $path: ") {
        rejected(status(method, path, "Sec-Fetch-Site" -> "cross-site")) shouldBe true
      }
    }
  }

  it should "let the site's own pages write" in {
    status("DELETE", "/api/me", "Sec-Fetch-Site" -> "same-origin") shouldBe 200
    status("POST", "/auth/logout", "Sec-Fetch-Site" -> "same-origin") shouldBe 200
  }

  it should "let the native apps and server-to-server callers write — they send no Sec-Fetch-Site" in {
    status("PUT", "/api/me/uk/hidden-films/Heat") shouldBe 200
    status("POST", "/auth/token", "Content-Type" -> "application/json") shouldBe 200
    status("POST", "/facebook/data-deletion", "Content-Type" -> "application/x-www-form-urlencoded") shouldBe 200
  }

  // Browsers that predate Fetch Metadata (Safari before 16.4) send no
  // Sec-Fetch-Site, but a cross-site write from one still names its page in
  // Origin (every POST/PUT/DELETE) or, failing that, Referer.
  it should "refuse a write whose Origin is foreign when the browser sends no Sec-Fetch-Site" in {
    rejected(status("POST", "/auth/sessions/revoke", "Origin" -> "https://evil.example")) shouldBe true
    rejected(status("DELETE", "/api/me", "Origin" -> "null")) shouldBe true
  }

  it should "refuse a write whose Referer is foreign when there is neither Sec-Fetch-Site nor Origin" in {
    rejected(status("PUT", "/api/me/state", "Referer" -> "https://evil.example/page")) shouldBe true
  }

  it should "let a write from the site's own origin through when the browser sends no Sec-Fetch-Site" in {
    status("POST", "/auth/logout", "Origin" -> "http://localhost") shouldBe 200
    status("PUT", "/api/me/state", "Referer" -> "http://localhost/poznan/") shouldBe 200
    status("POST", "/auth/logout", "Host" -> "kinowo.net", "X-Forwarded-Proto" -> "https",
      "Origin" -> "https://kinowo.net") shouldBe 200
  }

  it should "leave cross-site reads and CORS preflights alone" in {
    status("GET", "/api/catalog", "Sec-Fetch-Site" -> "cross-site") shouldBe 200
    status("HEAD", "/", "Sec-Fetch-Site" -> "cross-site") shouldBe 200
    status("OPTIONS", "/api/me", "Sec-Fetch-Site" -> "cross-site") shouldBe 200
  }
}
