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

  it should "leave cross-site reads and CORS preflights alone" in {
    status("GET", "/api/catalog", "Sec-Fetch-Site" -> "cross-site") shouldBe 200
    status("HEAD", "/", "Sec-Fetch-Site" -> "cross-site") shouldBe 200
    status("OPTIONS", "/api/me", "Sec-Fetch-Site" -> "cross-site") shouldBe 200
  }
}
