package controllers

import models.{City, Country}
import play.api.mvc._

/**
 * The bare `/` entry point. There is no city in the path here, so:
 *   - a returning visitor (carrying the `city` cookie a page render set) is
 *     bounced straight to `/{slug}/`;
 *   - everyone else gets the city-selection screen, which also tries browser
 *     geolocation client-side and redirects to the nearest supported city
 *     within 100 km (see `landing.scala.html`).
 *
 * Hard cut: the old unprefixed repertoire URLs no longer exist; this is the
 * only thing served at `/`.
 */
class LandingController(cc: ControllerComponents)(implicit messages: play.api.i18n.Messages) extends AbstractController(cc) {
  def index(): Action[AnyContent] = Action { request =>
    request.cookies.get("city").map(_.value).flatMap(City.bySlug) match {
      case Some(c) => Redirect(s"/${c.slug}/")
      case None    => Ok(views.html.landing(Country.fromEnv.allSorted))
    }
  }
}
