package modules

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Country, DeploymentOrigins}
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

  /** One deployment's two redirect filters, in production's order (the front door
   *  first), over a stand-in router that, like the
   *  real one, 301s a bare `/{slug}` to `/{slug}/` and answers 200 otherwise,
   *  counting into a registry of its own. */
  private final class Deployment(country: Country) {
    val registry = new PrometheusRegistry()
    private val legacy = new LegacyAddressMetrics(registry, country.code, new MutableClock(Instant.parse("2026-10-13T12:00:00Z")))
    private val chain: RequestHeader => Future[Result] = {
      val renamed   = new RenamedCityRedirectFilter(country, legacy)
      val frontDoor = new FrontDoorRedirectFilter(country, legacy, controllers.FrontDoorMessages(testsupport.TestMessages.messagesApi))
      val router: RequestHeader => Future[Result] = rh =>
        Future.successful(
          if (rh.path.count(_ == '/') == 1 && rh.path.length > 1) Results.MovedPermanently(rh.path + "/")
          else Results.Ok)
      frontDoor(renamed(router))
    }
    def get(path: String, agent: String, host: String = "localhost"): Result =
      Await.result(chain(FakeRequest("GET", path).withHeaders("Host" -> host, "User-Agent" -> agent)), SpecTimeouts.Io)

    /** `get`, following this deployment's own 301s the way a browser would. */
    def follow(path: String, agent: String, host: String = "localhost"): Result = {
      val first = get(path, agent, host)
      first.header.headers.get("Location").filter(_ => first.header.status == 301)
        .filter(to => Country.switchable.filter(_ != country).forall(c => c.pathPrefix.isEmpty || !to.startsWith(c.pathPrefix + "/")))
        .fold(first)(follow(_, agent, host))
    }

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
    front.get("/york/", Browser, host = DeploymentOrigins.apexHost)      // the table changes it: the US, not the UK
    front.get("/kent/", Browser, host = DeploymentOrigins.apexHost)      // in the table, but the UK either way
    front.get("/leeds/", Ios, host = DeploymentOrigins.apexHost)         // not in the table: the first in order
    front.get("/york/", Browser, host = "kinowo.net")          // off the apex: nothing
    front.get("/poznan/", Bot)                                 // Poland's own page on its own host: nothing
    front.hits shouldBe Map(
      ("legacy:front_door_old_owner", "browser") -> 1.0,
      ("legacy:front_door_new", "browser")       -> 1.0,
      ("legacy:front_door_new", "ios_app")       -> 1.0)
  }

  it should "count a bare slug as old-owner only where the table changes the answer" in {
    // York: the UK has a page and comes first in order, but the table says the US.
    Country.bareSlugTarget("york").map(t => t.country.code -> t.tableChangedAnswer) shouldBe Some("us" -> true)
    // Kent: in the table, and the UK is the one the rule without it would pick too.
    Country.bareSlugTarget("kent").map(t => t.country.code -> t.tableChangedAnswer) shouldBe Some("uk" -> false)
  }

  it should "not count a Polish place's 404 on the apex, which is no legacy address" in {
    val front = new Deployment(Country.Poland)
    front.get("/warszawa/", Browser, host = DeploymentOrigins.apexHost).header.status shouldBe 404
    front.get("/warszawa", Browser, host = DeploymentOrigins.apexHost).header.status shouldBe 404
    // A renamed Polish slug neither: the apex does not 301 it onto a page it then 404s.
    val renamed = Country.Poland.renamedSlugs.keys.find(s => Country.bareSlugTarget(s).exists(_.country == Country.Poland))
      .getOrElse(fail("no renamed Polish slug"))
    front.get(s"/$renamed/", Ios, host = DeploymentOrigins.apexHost).header.status shouldBe 404
    front.hits shouldBe empty
  }

  it should "count a slash-less legacy address once, not again for the slash its redirect adds" in {
    val front = new Deployment(Country.Poland)
    front.follow("/york", Browser, host = DeploymentOrigins.apexHost).header.headers("Location") shouldBe "/us/york/"
    front.hits shouldBe Map(("legacy:front_door_old_owner", "browser") -> 1.0)
    val uk = new Deployment(Country.UnitedKingdom)
    uk.follow("/uk/kent", Ios).header.status shouldBe 200
    uk.hits shouldBe Map(("legacy:retired_slug", "ios_app") -> 1.0)
  }

  it should "not count the synthetic probe" in {
    val uk = new Deployment(Country.UnitedKingdom)
    uk.get("/uk/kent/", s"${services.metrics.WebHttpMetrics.SyntheticProbeAgent}1.0")
    uk.hits shouldBe empty
  }
}
