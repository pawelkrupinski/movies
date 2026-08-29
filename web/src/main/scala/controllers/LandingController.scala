package controllers

import models.{City, Country}
import play.api.mvc._

/**
 * The bare `/` entry point, which serves two different screens depending on the
 * host it was asked on:
 *
 *   - the BRAND FRONT DOOR (`showtimes.cc`, no country subdomain) gets the
 *     country picker — every deployed country, each linking to its own domain.
 *     This is the ONLY host-dependent routing in the app; everything else takes
 *     its country from `KINOWO_COUNTRY` once at boot.
 *   - a country's own host (`kinowo.net`, `uk.showtimes.cc`, …) gets the city
 *     screen: a returning visitor carrying the `city` cookie a page render set
 *     is bounced straight to `/{slug}/`, and everyone else gets the
 *     city-selection screen, which also tries browser geolocation client-side
 *     and redirects to the nearest supported city within 100 km (see
 *     `landing.scala.html`).
 *
 * The cookie bounce sits BELOW the apex check deliberately. A `city` cookie is
 * host-scoped (the client sets it with no `domain`), so the apex should never
 * carry one — but if it ever did, bouncing to `showtimes.cc/poznan/` would serve
 * a city path off a front door that has no repertoire behind it.
 *
 * Hard cut: the old unprefixed repertoire URLs no longer exist; these are the
 * only things served at `/`.
 */
class LandingController(cc: ControllerComponents)(implicit messages: play.api.i18n.Messages) extends AbstractController(cc) {
  def index(): Action[AnyContent] = Action { request =>
    if (Country.servesApex(PageMeta.host(request)))
      Ok(views.html.landingCountries(Country.switchable))
    else
      request.cookies.get("city").map(_.value).flatMap(City.bySlug) match {
        case Some(c) => Redirect(s"/${c.slug}/")
        case None    => Ok(views.html.landing(Country.fromEnv.allSorted))
      }
  }
}
