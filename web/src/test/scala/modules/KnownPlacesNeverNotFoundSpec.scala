package modules

import controllers.{MovieController, TestMovieController}
import models.Country
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.{EssentialAction, RequestHeader, Result, Results}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.metrics.{LegacyAddressMetrics, SpecLegacyAddressMetrics}
import tools.SpecTimeouts

import scala.concurrent.{Await, Future}

/**
 * A place the site knows never 404s, in any country: every page slug, every slug
 * a city used to answer at (`Country.renamedSlugs`) and every other name of a
 * place a page covers (`Country.slugAliases`), with and without its trailing
 * slash, through the deployment's real filters and the real city actions —
 * answering 200, or a 301 that lands on a 200.
 *
 * The router here binds the three city shapes the routes file does (`/:city`,
 * `/:city/`, `/:city/api/cinemas`) to the real `MovieController`; everything a
 * request meets before them is the production filter pair. Every address lands
 * on the real `/:city/` page (`MovieController.index`, through `renderIndex`), so
 * a page that resolves but fails to render fails here too; over an empty read
 * model all ~12,000 addresses render in a few seconds.
 */
class KnownPlacesNeverNotFoundSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private given system: ActorSystem = ActorSystem("known-places-spec")
  private given mat: Materializer   = Materializer(system)

  override def afterAll(): Unit = Await.result(system.terminate(), SpecTimeouts.Io): Unit

  /** One country's deployment: its two redirect filters over a router of the
   *  city routes, mounted at its own `mountPath`. */
  private final class Deployment(country: Country, legacy: LegacyAddressMetrics = SpecLegacyAddressMetrics.fresh()) {
    private val controller: MovieController = TestMovieController.build(Nil, servingCountry = country)._1
    private val mount = country.mountPath.stripSuffix("/")

    private def route(rh: RequestHeader): Future[Result] = {
      val rest = rh.path.stripPrefix(mount + "/")
      val action: EssentialAction = rest.split("/", -1).toSeq match {
        case Seq(city)                 => controller.indexNoTrailingSlash(city)
        case Seq(city, "")             => controller.index(city)
        case Seq(city, "api", "cinemas") => controller.apiCinemas(city)
        case _                         => EssentialAction(_ => play.api.libs.streams.Accumulator.done(Results.NotFound))
      }
      action(rh).run()
    }

    private val chain: RequestHeader => Future[Result] = {
      val renamed   = new RenamedCityRedirectFilter(country, legacy)
      val frontDoor = new FrontDoorRedirectFilter(country, legacy)
      rh => renamed(frontDoor(route))(rh)
    }

    def get(path: String, host: String = "localhost"): Result =
      Await.result(chain(FakeRequest(GET, path).withHeaders("Host" -> host)), SpecTimeouts.Io)

    def path(slug: String, slash: Boolean): String = s"$mount/$slug${if (slash) "/" else ""}"
  }

  private def everySlug(country: Country): Seq[String] =
    (country.cities.map(_.slug) ++ country.renamedSlugs.keys ++ country.slugAliases.keys).distinct.sorted

  private def walk(d: Deployment, slugs: Seq[String]): Seq[String] =
    for {
      slug  <- slugs
      slash <- Seq(true, false)
      problem <- {
        val first = d.get(d.path(slug, slash))
        first.header.status match {
          case OK => None
          case MOVED_PERMANENTLY =>
            val to = first.header.headers.getOrElse(LOCATION, "")
            val landed = d.get(to)
            Option.when(landed.header.status != OK)(s"${d.path(slug, slash)} → $to → ${landed.header.status}")
          case other => Some(s"${d.path(slug, slash)} → $other")
        }
      }.toSeq
    } yield problem

  Country.switchable.foreach { country =>
    s"Every known slug of ${country.code}" should "answer 200, or 301 onto a page that answers 200" in {
      val failures = walk(new Deployment(country), everySlug(country))
      withClue(failures.take(20).mkString("\n")) { failures shouldBe empty }
    }
  }

  it should "send a page's own slash-less address to its listing, and a town on a cluster to that page" in {
    val uk = new Deployment(Country.UnitedKingdom)
    uk.get("/uk/york").header.headers(LOCATION) shouldBe "/uk/york/"
    uk.get("/uk/harrogate/").header.status shouldBe OK
    val ilkley = Country.UnitedKingdom.slugAliases("ilkley")
    uk.get("/uk/ilkley").header.headers(LOCATION) shouldBe s"/uk/$ilkley/"
    // Unknown is still unknown.
    uk.get("/uk/atlantis/").header.status shouldBe NOT_FOUND
  }

  "The brand front door" should "send a bare slug of exactly one mounted country to that country's page" in {
    val front = new Deployment(Country.Poland)
    // Harrogate is a UK page and nothing else's.
    front.get("/harrogate/", host = Country.apexHost).header.headers(LOCATION) shouldBe "/uk/harrogate/"
    // A Polish page lives on kinowo.net: the apex sends it there, sub-pages and query included,
    // rather than serve a duplicate of it under showtimes.cc.
    front.get("/poznan/", host = Country.apexHost).header.headers(LOCATION) shouldBe "https://kinowo.net/poznan/"
    front.get("/poznan", host = Country.apexHost).header.headers(LOCATION) shouldBe "https://kinowo.net/poznan"
    front.get("/poznan/film/dune?d=1", host = Country.apexHost).header.headers(LOCATION) shouldBe
      "https://kinowo.net/poznan/film/dune?d=1"
    // The front door's own root is not sent away.
    front.get("/", host = Country.apexHost).header.headers.get(LOCATION) shouldBe None
    // On kinowo.net, Poland's own host, it is the page.
    front.get("/poznan/", host = "kinowo.net").header.status shouldBe OK
    // Off the apex — kinowo.net — nothing is redirected.
    front.get("/harrogate/", host = "kinowo.net").header.status shouldBe NOT_FOUND
  }

  it should "send a bare slug to its old owner, else the one country that has it, else the first in order" in {
    val front = new Deployment(Country.Poland)
    front.get("/york/", host = Country.apexHost).header.headers(LOCATION) shouldBe "/us/york/"      // the old owner
    front.get("/boston/", host = Country.apexHost).header.headers(LOCATION) shouldBe "/us/boston/"
    // A retired UK county page, in one hop to the page it retired into.
    val somerset = Country.UnitedKingdom.resolve("somerset").map(_.slug).getOrElse(fail("somerset"))
    front.get("/somerset/", host = Country.apexHost).header.headers(LOCATION) shouldBe s"/uk/$somerset/"
    front.get("/leeds/", host = Country.apexHost).header.headers(LOCATION) shouldBe "/uk/leeds/"    // new, UK first
  }

  /** The documented rule, computed here rather than through the filter's own
   *  `Country.bareSlugTarget`: the checked-in table's owner, else the first country,
   *  in `Country.switchable`'s order, that answers to the slug. */
  private def expectedOwner(slug: String): Option[Country] =
    models.LegacySlugOwners.owners.get(slug).flatMap(Country.byCode)
      .orElse(Country.switchable.find(_.resolve(slug).isDefined))

  it should "send every slug any country answers to where the table and its fallback say" in {
    val front = new Deployment(Country.Poland)
    val failures = for {
      country <- Country.switchable
      slug    <- everySlug(country)
      owner   <- expectedOwner(slug).toSeq
      result   = front.get(s"/$slug/", host = Country.apexHost)
      location = result.header.headers.get(LOCATION)
      problem <- (
        if (owner == Country.Poland)
          // Poland's own: to kinowo.net, Poland's host — directly, or through Poland's own
          // 301 from an alias or renamed slug onto its page, which then goes there.
          Option.when(!location.exists(to => to.startsWith("https://kinowo.net/") ||
              front.get(to, host = Country.apexHost).header.headers.get(LOCATION).contains(s"https://kinowo.net$to")))(
            s"/$slug/ -> $location, not kinowo.net")
        else {
          val expected = owner.resolve(slug).map(city => s"${owner.pathPrefix}/${city.slug}/")
          Option.when(expected.isEmpty || location != expected)(
            s"/$slug/ -> ${result.header.status} $location, not $expected")
        }).toSeq
    } yield problem
    withClue(failures.take(20).mkString("\n")) { failures.distinct shouldBe empty }
  }
}
