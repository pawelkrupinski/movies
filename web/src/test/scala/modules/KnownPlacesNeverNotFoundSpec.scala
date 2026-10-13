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
 * request meets before them is the production filter pair. The whole walk checks
 * a listing through `/:city/api/cinemas`, which resolves the city exactly as
 * `/:city/` does (`withCity` → `ServedCity`) — rendering every one of ~12,000
 * addresses' pages would take minutes — and a second walk renders the real
 * `/:city/` page (`MovieController.index`) for a sample of every country plus
 * every slug the front door rules on and every qualified slug that 301s onto a
 * bare one, so a page that resolves but fails to render is caught too.
 */
class KnownPlacesNeverNotFoundSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  private given system: ActorSystem = ActorSystem("known-places-spec")
  private given mat: Materializer   = Materializer(system)

  override def afterAll(): Unit = Await.result(system.terminate(), SpecTimeouts.Io): Unit

  /** One country's deployment: its two redirect filters over a router of the
   *  city routes, mounted at its own `mountPath`. */
  private final class Deployment(country: Country, renderListing: Boolean = false) {
    private val controller: MovieController = TestMovieController.build(Nil, servingCountry = country)._1
    private val mount = country.mountPath.stripSuffix("/")

    private def route(rh: RequestHeader): Future[Result] = {
      val rest = rh.path.stripPrefix(mount + "/")
      val action: EssentialAction = rest.split("/", -1).toSeq match {
        case Seq(city)                 => controller.indexNoTrailingSlash(city)
        case Seq(city, "")             => if (renderListing) controller.index(city) else controller.apiCinemas(city)
        case Seq(city, "api", "cinemas") => controller.apiCinemas(city)
        case _                         => EssentialAction(_ => play.api.libs.streams.Accumulator.done(Results.NotFound))
      }
      action(rh).run()
    }

    private val chain: RequestHeader => Future[Result] = {
      val renamed   = new RenamedCityRedirectFilter(country)
      val frontDoor = new FrontDoorRedirectFilter(country)
      rh => renamed(frontDoor(route))(rh)
    }

    def get(path: String, host: String = "localhost"): Result =
      Await.result(chain(FakeRequest(GET, path).withHeaders("Host" -> host)), SpecTimeouts.Io)

    def path(slug: String, slash: Boolean): String = s"$mount/$slug${if (slash) "/" else ""}"
  }

  private def everySlug(country: Country): Seq[String] =
    (country.cities.map(_.slug) ++ country.renamedSlugs.keys ++ country.slugAliases.keys).distinct.sorted

  /** The slugs whose real `/:city/` page is rendered: every 20th known slug, every
   *  slug the front door rules on, and every qualified slug (`york-north-yorkshire`)
   *  that 301s onto the bare page it qualifies. */
  private def renderedSlugs(country: Country): Seq[String] = {
    val all = everySlug(country)
    val redirects = country.renamedSlugs ++ country.slugAliases
    val qualified = redirects.collect { case (from, to) if from.startsWith(to + "-") => from }
    val contested = Country.frontDoorContestedSlugs.filter(country.resolve(_).isDefined)
    (all.grouped(20).map(_.head) ++ contested ++ qualified).toSeq.distinct.sorted
  }

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

    it should "render its real listing page for a sample, the front door's slugs and the qualified ones" in {
      val slugs = renderedSlugs(country)
      slugs.size should be > 10
      val failures = walk(new Deployment(country, renderListing = true), slugs)
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
    // A Polish page stays Poland's, on the apex as on kinowo.net.
    front.get("/poznan/", host = Country.apexHost).header.status shouldBe OK
    // Off the apex — kinowo.net — nothing is redirected.
    front.get("/harrogate/", host = "kinowo.net").header.status shouldBe NOT_FOUND
  }

  it should "send a slug several countries answer to the country with the most populous town of that name" in {
    val front = new Deployment(Country.Poland)
    front.get("/york/", host = Country.apexHost).header.headers(LOCATION) shouldBe "/uk/york/"      // 156k vs 44k
    front.get("/boston/", host = Country.apexHost).header.headers(LOCATION) shouldBe "/us/boston/"  // Massachusetts
  }

  it should "send every slug a mounted country answers to somewhere that country serves" in {
    val front = new Deployment(Country.Poland)
    val mounted = Country.mountedUnderApex
    val failures = for {
      country <- mounted
      slug    <- everySlug(country) if Country.Poland.resolve(slug).isEmpty
      result   = front.get(s"/$slug/", host = Country.apexHost)
      problem <- Option.when(result.header.status != MOVED_PERMANENTLY)(s"/$slug/ → ${result.header.status}")
        .orElse {
          val to = result.header.headers(LOCATION)
          val (code, page) = to.stripPrefix("/").stripSuffix("/").span(_ != '/')
          val owner = mounted.find(_.code == code)
          Option.when(!owner.exists(_.bySlug.contains(page.stripPrefix("/"))))(s"/$slug/ → $to, no such page")
        }.toSeq
    } yield problem
    withClue(failures.take(20).mkString("\n")) { failures shouldBe empty }
  }

  it should "have a recorded winner for every slug several mounted countries answer to" in {
    val contested = Country.mountedUnderApex.flatMap(everySlug).groupBy(identity).collect {
      case (slug, owners) if Country.mountedUnderApex.count(_.resolve(slug).isDefined) > 1 => slug
    }
    contested.filterNot(Country.frontDoorContestedSlugs) shouldBe empty
  }

}
