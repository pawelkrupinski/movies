package modules.webwiring

import controllers.{CatalogController, ClientSupportController, EncodedResponseCache, HealthController, LandingController, LanguageController, LegalController, MovieController, SupportController, WellKnownController}
import modules.Wiring

/** ── The public site ───────────────────────────────────────────────────────
 *  The controllers a visitor (or the mobile apps) reach without signing in:
 *  the landing page, the per-city listing + film pages, the app catalog, the
 *  legal/support pages and the operational `/health` + `.well-known` answers. */
trait ControllersWiring { self: Wiring =>

  // The deployment's DEFAULT `Lang` — Poland → pl → default `messages`; other
  // countries → their own bundle. `LandingController`/`MovieController`
  // resolve a `Messages` per request now (`WebLangResolver`), layering a
  // visitor's own pick/`Accept-Language` over this rather than rendering
  // every visitor in it unconditionally.
  lazy val deploymentLang: play.api.i18n.Lang = play.api.i18n.Lang(models.Country.fromEnv.language)

  // The fixed deployment `Messages` — still what `DebugController` and
  // `RetiredSiteController`-adjacent, ops/crawler-facing renders use (see
  // `MovieController`'s own doc comment on `requestMessages` for the full
  // list of pages deliberately left on this rather than the per-request one).
  implicit lazy val deploymentMessages: play.api.i18n.Messages =
    messagesApi.preferred(Seq(deploymentLang))

  lazy val landingController = new LandingController(controllerComponents, models.Country.fromEnv)
  lazy val languageController = new LanguageController(controllerComponents)
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
