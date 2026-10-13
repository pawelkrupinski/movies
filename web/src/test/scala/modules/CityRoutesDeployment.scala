package modules

import controllers.{MovieController, TestMovieController}
import models.Country
import org.apache.pekko.stream.Materializer
import play.api.mvc.{EssentialAction, RequestHeader, Result, Results}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.metrics.{LegacyAddressMetrics, SpecLegacyAddressMetrics}
import tools.SpecTimeouts

import scala.concurrent.{Await, Future}

/**
 * One country's deployment as far as a city address goes: its two redirect filters
 * over a router of the city routes, mounted at its own `mountPath`, in production's
 * order (`AppLoader.filterChain`: the front door first).
 *
 * The router binds the city shapes the routes file does that a visitor or an app
 * reaches — `/:city`, `/:city/`, the three `/:city/api/…` endpoints the apps call
 * and the film shapes a share link carries (`/movie/:slug`, `/movie?title=`, the
 * pre-rename `/film…`) — to the real `MovieController`; everything a request meets
 * before them is the production filter pair. Over the `films` it is handed, by
 * default none, so thousands of addresses render in seconds.
 */
final class CityRoutesDeployment(country: Country,
                                 legacy: LegacyAddressMetrics = SpecLegacyAddressMetrics.fresh(),
                                 films: Seq[(String, Option[Int], models.MovieRecord)] = Nil)
                                (using Materializer) {
  private val controller: MovieController = TestMovieController.build(films, servingCountry = country)._1
  private val mount = country.mountPath.stripSuffix("/")

  private def route(rh: RequestHeader): Future[Result] = {
    val rest = rh.path.stripPrefix(mount + "/")
    val action: EssentialAction = rest.split("/", -1).toSeq match {
      case Seq(city)                      => controller.indexNoTrailingSlash(city)
      case Seq(city, "")                  => controller.index(city)
      case Seq(city, "api", "repertoire") => controller.apiRepertoire(city)
      case Seq(city, "api", "details")    => controller.apiDetails(city)
      case Seq(city, "api", "cinemas")    => controller.apiCinemas(city)
      case Seq(city, "movie")             => controller.film(city, rh.getQueryString("title").getOrElse(""))
      case Seq(city, "movie", slug)       => controller.filmBySlug(city, slug)
      case Seq(city, "film")              => controller.filmLegacy(city)
      case city +: "film" +: rest         => controller.filmSubPathLegacy(city, rest.mkString("/"))
      case _                              => EssentialAction(_ => play.api.libs.streams.Accumulator.done(Results.NotFound))
    }
    action(rh).run()
  }

  private val chain: RequestHeader => Future[Result] = {
    val renamed   = new RenamedCityRedirectFilter(country, legacy)
    val frontDoor = new FrontDoorRedirectFilter(country, legacy, controllers.FrontDoorMessages(testsupport.TestMessages.messagesApi))
    rh => frontDoor(renamed(route))(rh)
  }

  def get(path: String, host: String = "localhost", headers: Seq[(String, String)] = Nil): Result =
    Await.result(chain(FakeRequest(GET, path).withHeaders(("Host" -> host) +: headers*)), SpecTimeouts.Io)

  def path(slug: String, slash: Boolean): String = s"$mount/$slug${if (slash) "/" else ""}"

  /** Where `path` lands — itself on a 200, the `Location` of up to `hops` 301s that
   *  ends in a 200 — or what went wrong instead. */
  def landsOnOk(path: String, hops: Int = 1): Either[String, String] = {
    val first = get(path)
    first.header.status match {
      case OK => Right(path)
      case MOVED_PERMANENTLY if hops > 0 =>
        val to = first.header.headers.getOrElse(LOCATION, "")
        landsOnOk(to, hops - 1).left.map(problem => s"$path → $problem")
      case other => Left(s"$path → $other")
    }
  }
}
