package controllers

import models.City
import play.api.mvc._

import java.time.LocalDate

/** The per-city JSON the mobile apps (and the web page's own fetches) read:
 *  `/{city}/api/repertoire`, `/api/details` and `/api/cinemas`. Rendered for
 *  nobody, like the listing, so every payload is offered to the shared cache
 *  under the city's own validator (`CityRequests.conditionalResponse`). */
class MovieApiController(cc: ControllerComponents,
                         cityRequests: CityRequests,
                         movieControllerService: MovieControllerService,
                        ) extends AbstractController(cc) {

  import cityRequests.withCity

  // A city's JSON payloads, each film's object serialized once and kept.
  private[controllers] val jsonPayloads = new CityJsonPayloads

  /** Conditional-GET wrapper for the JSON API endpoints — the same mechanism as
   *  the HTML pages (see [[ConditionalResponse.serve]]): a current `If-Modified-Since`
   *  yields a bodiless 304 (what warm mobile clients hit), otherwise the payload
   *  is served from the shared gzip cache under `RevalidatedAnywhere`, so the
   *  edge may hold it too. Both the listing and the details payload track the
   *  same city's stamp, so a 304 on one is a 304 on the other. */
  private def conditionalJson(request: Request[AnyContent], city: City, cacheKey: String = "")(body: => ResponseBody): Result =
    cityRequests.conditionalResponse.serve(request, "application/json", CachePolicy.RevalidatedAnywhere,
                                           cacheKey = cacheKey, city = Some(city))(body)

  /** Lean listing — everything the grid + filters need, no heavy detail text.
   *  Latency-sensitive; clients hit this on the critical path. */
  def apiRepertoire(city: String, days: Option[Int] = None): Action[AnyContent] = Action { request =>
    withCity(city, request) { c =>
      val window = CityJsonPayloads.dayWindow(days)
      conditionalJson(request, c, CityJsonPayloads.windowCacheKey(window)) {
        jsonPayloads.repertoire(c, today(c), movieControllerService.toSchedules(c), window)
      }
    }
  }

  /** Detail-only payload (synopsis + trailers), keyed by title. Clients fetch
   *  this in parallel with the listing and merge; keeping it off
   *  `/{city}/api/repertoire` halves the listing's gzip size. */
  def apiDetails(city: String): Action[AnyContent] = Action { request =>
    withCity(city, request) { c =>
      conditionalJson(request, c)(jsonPayloads.details(c, today(c), movieControllerService.toSchedules(c)))
    }
  }

  /** The city's cinema universe + area grouping (static). Mobile fetches this
   *  once per city to render the collapsible, per-area cinema filter — the
   *  counterpart of the server-side `CINEMA_AREAS` the web page is handed. */
  def apiCinemas(city: String): Action[AnyContent] = Action { request =>
    withCity(city, request)(c => conditionalJson(request, c)(CityJsonPayloads.cinemas(c, today(c))))
  }

  /** The city's own calendar day, off the injected clock. */
  private def today(city: City): LocalDate = movieControllerService.nowIn(city).toLocalDate
}
