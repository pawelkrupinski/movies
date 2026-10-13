package modules

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Country
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.{RequestHeader, Result, Results}
import play.api.test.FakeRequest
import services.metrics.{LegacyAddressMetrics, PrometheusExposition}
import tools.{MutableClock, SpecTimeouts}

import java.time.Instant
import scala.concurrent.{Await, Future}

/**
 * Every legacy city address the redirect filters serve is counted once, by its
 * kind and the client that asked — the signal for when the old-owner table and
 * the legacy redirects can go.
 */
class LegacyAddressUsageSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private given system: ActorSystem = ActorSystem("legacy-address-spec")
  private given mat: Materializer   = Materializer(system)

  override def afterAll(): Unit = Await.result(system.terminate(), SpecTimeouts.Io): Unit

  private val Ios     = "KinowoIOS/1.0"
  private val Android = "KinowoAndroid/1.0"
  private val Browser = "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0)"
  private val Bot     = "Mozilla/5.0 (compatible; Googlebot/2.1)"

  /** One deployment's two redirect filters over a router that answers 200,
   *  counting into a registry of its own. */
  private final class Deployment(country: Country) {
    val registry = new PrometheusRegistry()
    private val legacy = new LegacyAddressMetrics(registry, country.code, new MutableClock(Instant.parse("2026-10-13T12:00:00Z")))
    private val chain: RequestHeader => Future[Result] = {
      val renamed   = new RenamedCityRedirectFilter(country, legacy)
      val frontDoor = new FrontDoorRedirectFilter(country, legacy)
      renamed(frontDoor(_ => Future.successful(Results.Ok)))
    }
    def get(path: String, agent: String, host: String = "localhost"): Result =
      Await.result(chain(FakeRequest("GET", path).withHeaders("Host" -> host, "User-Agent" -> agent)), SpecTimeouts.Io)

    /** kind → client → hits, from the exposition. */
    def hits: Map[(String, String), Double] =
      """kinowo_web_legacy_address_hits_total\{client="([^"]+)",country="[a-z]+",kind="([^"]+)"\} (\S+)""".r
        .findAllMatchIn(PrometheusExposition.render(registry))
        .map(m => (m.group(2), m.group(1)) -> m.group(3).toDouble).toMap

    def lastCalled: Set[(String, String)] =
      """kinowo_web_legacy_address_last_called_seconds\{client="([^"]+)",country="[a-z]+",kind="([^"]+)"\}""".r
        .findAllMatchIn(PrometheusExposition.render(registry)).map(m => (m.group(2), m.group(1))).toSet
  }

  "A legacy city address" should "be counted once per hit, by its kind and the client that asked" in {
    val uk = new Deployment(Country.UnitedKingdom)
    uk.get("/uk/kent/", Ios)                         // the UK's retired county page
    uk.get("/uk/kent/movie/dune", Ios)
    uk.get("/uk/ilkley/", Android)                   // a town on a cluster's page
    uk.get("/uk/york-north-yorkshire/", Browser)     // a page's qualified slug
    uk.get("/uk/york/", Browser)                     // a page: not legacy
    uk.hits shouldBe Map(
      ("legacy:retired_slug", "ios_app")    -> 2.0,
      ("legacy:town_alias", "android_app")  -> 1.0,
      ("legacy:town_alias", "browser")      -> 1.0)
    uk.lastCalled shouldBe uk.hits.keySet

    val us = new Deployment(Country.UnitedStates)
    us.get("/us/birmingham-alabama/", Bot)
    us.hits shouldBe Map(("legacy:qualified_to_bare", "bot") -> 1.0)

    val front = new Deployment(Country.Poland)
    front.get("/york/", Browser, host = Country.apexHost)      // the old owner's: the US
    front.get("/leeds/", Ios, host = Country.apexHost)         // not in the table: the first in order
    front.get("/york/", Browser, host = "kinowo.net")          // off the apex: nothing
    front.get("/poznan/", Bot)                                 // Poland's own page on its own host: nothing
    front.hits shouldBe Map(
      ("legacy:front_door_old_owner", "browser") -> 1.0,
      ("legacy:front_door_new", "ios_app")       -> 1.0)
  }

  it should "not count the synthetic probe" in {
    val uk = new Deployment(Country.UnitedKingdom)
    uk.get("/uk/kent/", s"${services.metrics.WebHttpMetrics.SyntheticProbeAgent}1.0")
    uk.hits shouldBe empty
  }
}
