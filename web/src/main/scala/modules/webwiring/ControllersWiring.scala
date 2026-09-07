package modules.webwiring

import controllers.{CatalogController, ClientSupportController, EncodedResponseCache, HealthController, LandingController, LegalController, MovieController, SupportController, WellKnownController}
import modules.Wiring

/** ── The public site ───────────────────────────────────────────────────────
 *  The controllers a visitor (or the mobile apps) reach without signing in:
 *  the landing page, the per-city listing + film pages, the app catalog, the
 *  legal/support pages and the operational `/health` + `.well-known` answers. */
trait ControllersWiring { self: Wiring =>

  // The single `Messages` this deployment renders with — fixed at boot from the
  // country's language (Poland → pl → default `messages`; other countries → en).
  // A web deployment serves ONE country, so the locale never varies per request;
  // controllers inject this into their Twirl views instead of deriving a Lang
  // from `Accept-Language`.
  implicit lazy val deploymentMessages: play.api.i18n.Messages =
    messagesApi.preferred(Seq(play.api.i18n.Lang(models.Country.fromEnv.language)))

  // View-rendering controllers take the deployment's fixed `Messages`
  // (`deploymentMessages`, implicit above) so their Twirl views resolve
  // `@messages("…")` in the country's language.
  lazy val landingController = new LandingController(controllerComponents, models.Country.fromEnv)
  lazy val encodedResponseCache = new EncodedResponseCache
  // Fetches + composites the per-film Open Graph share card. Its own poster
  // fetch (not the scraper's httoFetch) so slow cinema origins get a generous
  // connect budget instead of the fan-out's tight 5s.
  lazy val ogCardService     = new tools.OgCardService(new tools.HttpPosterFetch)
  lazy val cityOgCardService = new tools.CityOgCardService(new tools.HttpPosterFetch)

  lazy val movieController  = new MovieController(controllerComponents, movieControllerService, webReadModel, oauthProviders.keySet, environmentMode, encodedResponseCache, ogCardService, cityOgCardService)
  // Global country+city catalog for the mobile apps (`GET /api/catalog`), served
  // identically by every deployment — no per-country/read-model dependency.
  lazy val catalogController = new CatalogController(controllerComponents)
  lazy val clientSupportController = new ClientSupportController(controllerComponents)
  lazy val healthController = new HealthController(controllerComponents)
  lazy val wellKnownController = new WellKnownController(controllerComponents)
  lazy val legalController   = new LegalController(controllerComponents)
  lazy val supportController = new SupportController(controllerComponents)
}
