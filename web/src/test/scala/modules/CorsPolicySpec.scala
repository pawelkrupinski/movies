package modules

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.streams.Accumulator
import play.api.mvc.{EssentialAction, Result, Results}
import play.api.test.FakeRequest
import play.api.{ApplicationLoader, Environment}
import play.filters.cors.{CORSConfig, CORSFilter}

import scala.concurrent.duration._
import scala.concurrent.Await

/**
 * The CORS policy the deployed `application.conf` actually configures.
 *
 * Every state-changing `/api/me` and `/auth` route is `nocsrf` (the browser
 * JS calls them with `fetch`, not a form), so what keeps another origin from
 * driving them with a visitor's cookie is SameSite plus THIS policy. It allows
 * any origin — the public JSON (repertoire, catalog) is fine to read from
 * anywhere — but it must never let that origin do so WITH CREDENTIALS: Play's
 * default `supportsCredentials = true` combined with `allowedOrigins = null`
 * echoes any `Origin` back alongside `Access-Control-Allow-Credentials: true`,
 * which is a licence for any page to read `/api/me` (name, email) and to delete
 * the account behind a same-site-but-cross-origin request. Nothing of ours
 * needs credentialed cross-origin calls: every `fetch` in `shared.js` is
 * same-origin, and the native apps don't speak CORS at all.
 */
class CorsPolicySpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private implicit val sys: ActorSystem  = ActorSystem("cors-policy-spec")
  private implicit val mat: Materializer = Materializer(sys)

  override def afterAll(): Unit = sys.terminate()

  // The configuration the loader actually boots with — application.conf on
  // the classpath, over Play's reference.conf defaults.
  private val filter = new CORSFilter(CORSConfig.fromConfiguration(
    ApplicationLoader.Context.create(Environment.simple()).initialConfiguration))

  private def run(request: FakeRequest[?]): Result =
    Await.result(filter(EssentialAction(_ => Accumulator.done(Results.Ok("ok"))))(request).run(), 5.seconds)

  "the CORS policy" should "never allow a foreign origin to send credentials on a preflight" in {
    val preflight = run(FakeRequest("OPTIONS", "/api/me")
      .withHeaders(
        "Origin"                        -> "https://evil.example",
        "Access-Control-Request-Method" -> "DELETE"))

    preflight.header.headers.get("Access-Control-Allow-Credentials") shouldBe None
  }

  it should "never allow a foreign origin to read a credentialed response" in {
    val response = run(FakeRequest("GET", "/api/me")
      .withHeaders("Origin" -> "https://evil.example", "Cookie" -> "PLAY_SESSION=x"))

    response.header.headers.get("Access-Control-Allow-Credentials") shouldBe None
  }

  it should "still let any origin read the public JSON" in {
    val response = run(FakeRequest("GET", "/api/catalog").withHeaders("Origin" -> "https://example.org"))

    response.header.headers.get("Access-Control-Allow-Origin") shouldBe Some("*")
  }
}
