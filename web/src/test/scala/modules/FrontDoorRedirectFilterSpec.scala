package modules

import models.Country
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.http.DefaultHttpErrorHandler
import play.api.mvc.{RequestHeader, Result, Results}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.metrics.SpecLegacyAddressMetrics
import tools.SpecTimeouts

import scala.concurrent.{Await, Future}

/**
 * What the front door leaves to the router: the apex's own files and endpoints are
 * not places, so the 404 it gives a Polish place never reaches them.
 */
class FrontDoorRedirectFilterSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private given system: ActorSystem = ActorSystem("front-door-filter-spec")
  private given mat: Materializer   = Materializer(system)

  override def afterAll(): Unit = Await.result(system.terminate(), SpecTimeouts.Io): Unit

  // Production-mode (no dev details), like the deployed one.
  private val errorHandler = new DefaultHttpErrorHandler()
  private val filter = new FrontDoorRedirectFilter(Country.Poland, SpecLegacyAddressMetrics.fresh(), errorHandler)
  private val routed: RequestHeader => Future[Result] = _ => Future.successful(Results.Ok("routed"))

  private def get(path: String, host: String = Country.apexHost): Result =
    Await.result(filter(routed)(FakeRequest(GET, path).withHeaders("Host" -> host)), SpecTimeouts.Io)

  private def body(result: Result): String =
    Await.result(result.body.consumeData, SpecTimeouts.Io).utf8String

  "The front door" should "pass the apex's own root, files and endpoints through to the router" in {
    Seq("/", "/robots.txt", "/sitemap.xml", "/.well-known/apple-app-site-association", "/.well-known/assetlinks.json",
        "/assets/img/favicon.png", "/api/picker/uk", "/api/catalog", "/auth/sso/start").foreach { path =>
      withClue(path)(get(path).header.status shouldBe OK)
    }
  }

  it should "leave kinowo.net, Poland's own host, alone" in {
    get("/poznan/", host = "kinowo.net").header.status shouldBe OK
    get("/poznan/movie/dune", host = "kinowo.net").header.status shouldBe OK
  }

  it should "answer a Polish place on the apex with a 404, also under www" in {
    get("/poznan/").header.status shouldBe NOT_FOUND
    get("/poznan/", host = "www.showtimes.cc").header.status shouldBe NOT_FOUND
  }

  it should "answer it with the app's own 404 page, the one any unknown address gets, not an empty body" in {
    val result = get("/poznan/")
    val unknown = Await.result(errorHandler.onClientError(FakeRequest(GET, "/poznan/"), NOT_FOUND), SpecTimeouts.Io)
    body(result) should not be empty
    result.body.contentType shouldBe unknown.body.contentType
    body(result) shouldBe body(unknown)
  }
}
