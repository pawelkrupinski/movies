package controllers

import models.{City, Country}
import play.api.i18n.Lang
import play.api.mvc._

/**
 * The bare `/` entry point, which serves two different screens depending on
 * WHERE it was asked:
 *
 *   - the BRAND FRONT DOOR (`showtimes.cc/`, the bare apex) gets the country
 *     picker — every deployed country, each linking to its own base URL. This is
 *     the ONLY request-dependent country routing in the app; everything else
 *     takes its country from `KINOWO_COUNTRY` once at boot.
 *   - a country's own site (`kinowo.net/`, `showtimes.cc/uk/`, …) gets the city
 *     screen: a returning visitor carrying the `city` cookie a page render set
 *     is bounced straight to `{mount}/{slug}/`, and everyone else gets the
 *     city-selection screen, which also tries browser geolocation client-side
 *     and redirects to the nearest supported city within 100 km (see
 *     `landing.scala.html`).
 *
 * A visitor who arrived by NAMING this country — from the front door, or the
 * navbar's country switch — carries [[LandingController.PickCityQuery]], and
 * gets the city list itself: neither the cookie bounce nor the geolocation
 * redirect. Both would answer "show me Germany" with a city the visitor did not
 * ask for, one from a previous visit and one from where they happen to stand.
 * The mobile apps do the same (`CityGateStart.locate`).
 *
 * Which of the two this deployment can be is decided by its MOUNT POINT, not by
 * the host alone: since the Showtimes countries moved under `showtimes.cc/uk/`,
 * every one of their pages arrives on the apex host too, so only the country
 * mounted at `/` has a `/` that isn't already its own landing (see
 * `Country.servesApex`).
 *
 * The cookie bounce sits BELOW the apex check deliberately. A `city` cookie is
 * scoped to the deployment's mount point (the client sets it with no `domain`),
 * so the front door should never carry one — but if it ever did, bouncing to
 * `showtimes.cc/poznan/` would serve a city path off a front door that has no
 * repertoire behind it.
 *
 * Hard cut: the old unprefixed repertoire URLs no longer exist; these are the
 * only things served at `/`.
 */
class LandingController(cc: ControllerComponents, country: Country = Country.fromEnv)(implicit messages: play.api.i18n.Messages)
    extends AbstractController(cc) {

  /** The front door is BRAND chrome, not a country's site, so it is rendered in
   *  the brand's language rather than the language of whichever deployment the
   *  proxy points the apex at. It used to be English by accident — the apex sat
   *  on the UK pod — and the country that answers it is now the one mounted at
   *  the root, which is Poland. A Polish "Wybierz kraj" on `showtimes.cc` would
   *  be a regression nobody asked for. */
  private lazy val frontDoorMessages: play.api.i18n.Messages =
    cc.messagesApi.preferred(Seq(Lang("en")))

  def index(): Action[AnyContent] = Action { request =>
    if (country.servesApex(PageMeta.host(request)))
      Ok(views.html.landingCountries(Country.switchable)(using frontDoorMessages))
    else
      request.cookies.get("city").map(_.value).flatMap(City.bySlug) match {
        case Some(c) if !LandingController.picksCity(request) =>
          Redirect(s"${country.pathPrefix}/${c.slug}/")
        case _ => Ok(views.html.landing(country))
      }
  }
}

object LandingController {

  /** Query parameter marking a landing the visitor reached by choosing THIS
   *  country, rather than by typing its address or returning to it. The city
   *  screen honours it by asking rather than guessing — see the class doc.
   *
   *  Written into links by `landingCountries.scala.html` and by `shared.js`'s
   *  `countrySwitchTarget`, and read here and by `landing.scala.html`'s inline
   *  script, so the spelling is pinned by tests on both sides. */
  val PickCityParam = "pick"
  val PickCityValue = "city"
  val PickCityQuery = s"?$PickCityParam=$PickCityValue"

  def picksCity(request: RequestHeader): Boolean =
    request.getQueryString(PickCityParam).contains(PickCityValue)
}
