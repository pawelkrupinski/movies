package modules

import controllers.TestMovieController
import models.{Country, DeploymentOrigins}
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.{EssentialAction, RequestHeader, Result, Results}
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

  private val filter = new FrontDoorRedirectFilter(Country.Poland, SpecLegacyAddressMetrics.fresh(), controllers.FrontDoorMessages(testsupport.TestMessages.messagesApi))
  private val routed: RequestHeader => Future[Result] = _ => Future.successful(Results.Ok("routed"))

  private def get(path: String, host: String = DeploymentOrigins.apexHost): Result =
    Await.result(filter(routed)(FakeRequest(GET, path).withHeaders("Host" -> host)), SpecTimeouts.Io)

  /** The real chain for an apex request: the front door over a router that binds
   *  `/:city/` to the real `MovieController.index` — what answers an unknown slug. */
  private val cityRoute: RequestHeader => Future[Result] = {
    val controller = TestMovieController.build(Nil, servingCountry = Country.Poland)._1
    rh => {
      val action: EssentialAction = rh.path.stripPrefix("/").split("/", -1).toSeq match {
        case Seq(city, "") => controller.index(city)
        case _             => EssentialAction(_ => play.api.libs.streams.Accumulator.done(Results.Ok("routed")))
      }
      action(rh).run()
    }
  }

  private def throughChain(path: String): Result =
    Await.result(filter(cityRoute)(FakeRequest(GET, path).withHeaders("Host" -> DeploymentOrigins.apexHost)), SpecTimeouts.Io)

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

  it should "answer it exactly as an unknown city there is answered, not with a different 404 page" in {
    val polish  = throughChain("/poznan/")
    val unknown = throughChain("/xyzzy/")
    unknown.header.status shouldBe NOT_FOUND
    polish.header.status shouldBe unknown.header.status
    polish.body.contentType shouldBe unknown.body.contentType
    body(unknown) should include("xyzzy")
    body(polish) shouldBe body(unknown).replace("xyzzy", "poznan")
  }

  it should "answer it in English, the front door's language, though Poland's deployment serves it" in {
    val english = testsupport.TestMessages.forLang("en")("error.unknownCity", "xyzzy")
    english should not be testsupport.TestMessages.deployment("error.unknownCity", "xyzzy")
    body(throughChain("/xyzzy/")) shouldBe english
    body(throughChain("/poznan/")) shouldBe english.replace("xyzzy", "poznan")
  }

  it should "keep Poland's own host in Polish" in {
    val result = Await.result(cityRoute(FakeRequest(GET, "/xyzzy/").withHeaders("Host" -> "kinowo.net")), SpecTimeouts.Io)
    body(result) shouldBe testsupport.TestMessages.deployment("error.unknownCity", "xyzzy")
  }
}
