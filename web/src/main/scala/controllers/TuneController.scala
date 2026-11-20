package controllers

import play.api.Mode
import play.api.mvc.*

/** The dev-only visual-tuning pages, `/{city}/debug/tune` and its film-detail sibling:
 *  the real card and film partials over in-process sample films ([[TuneSamples]]), so
 *  the pages work regardless of cache state, inside a slider panel driving the CSS
 *  custom properties the production styles read. */
class TuneController(cc: ControllerComponents, environment: Mode, cityRequests: CityRequests)
  extends AbstractController(cc) {

  private given play.api.i18n.Messages = cityRequests.deploymentMessages

  /** The card tuning page: the real `_movieCard` partial(s) inside a `.tune-scope` wrapper. */
  def tune(city: String): Action[AnyContent] = Action { request =>
    // The dev gate first: in production the page does not exist for ANY city, so
    // the slug is not worth looking up (nor an "unknown city" answer worth giving).
    DevMode.gate(environment) {
      cityRequests.withCity(city, request) { implicit c =>
        Ok(views.html.tune(TuneSamples.films))
      }
    }
  }

  /** The film-detail tuning page — live sliders over the real `_filmDetailContent`
   *  for the title / meta / Seanse typography. */
  def tuneFilm(city: String): Action[AnyContent] = Action { request =>
    DevMode.gate(environment) {
      cityRequests.withCity(city, request) { implicit c =>
        Ok(views.html.tuneFilm(TuneSamples.film))
      }
    }
  }
}
