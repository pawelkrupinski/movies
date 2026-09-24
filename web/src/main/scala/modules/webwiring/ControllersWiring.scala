package modules.webwiring

import controllers.{CatalogController, ClientSupportController, EncodedResponseCache, HealthController, LandingController, LegalController, MovieController, SupportController, WellKnownController}
import modules.Wiring

/** ── The public site ───────────────────────────────────────────────────────
 *  The controllers a visitor (or the mobile apps) reach without signing in:
 *  the landing page, the per-city listing + film pages, the app catalog, the
 *  legal/support pages and the operational `/health` + `.well-known` answers. */
trait ControllersWiring { self: Wiring =>

  // The deployment's DEFAULT, and ONLY, `Lang` — Poland → pl → default
  // `messages`; other countries → their own bundle. `LandingController` /
  // `MovieController` render every visitor in this; an explicit language
  // pick swaps the visible copy client-side instead (`shared.js`'s
  // `applyLanguage`), never by re-rendering server-side.
  lazy val deploymentLang: play.api.i18n.Lang = play.api.i18n.Lang(models.Country.fromEnv.language)

  // The fixed deployment `Messages` — what every visitor-facing render uses,
  // plus `DebugController`/`RetiredSiteController`-adjacent, ops/crawler-facing
  // ones.
  implicit lazy val deploymentMessages: play.api.i18n.Messages =
    messagesApi.preferred(Seq(deploymentLang))

  lazy val landingController = new LandingController(controllerComponents, models.Country.fromEnv)
  lazy val encodedResponseCache = new EncodedResponseCache
  lazy val movieController  = new MovieController(controllerComponents, movieControllerService, webReadModel, oauthProviders.keySet, environmentMode, encodedResponseCache)
  // Global country+city catalog for the mobile apps (`GET /api/catalog`), served
  // identically by every deployment — no per-country/read-model dependency.
  lazy val catalogController = new CatalogController(controllerComponents)
  lazy val clientSupportController = new ClientSupportController(controllerComponents)
  lazy val healthController = new HealthController(controllerComponents)
  lazy val wellKnownController = new WellKnownController(controllerComponents)
  lazy val legalController   = new LegalController(controllerComponents)
  lazy val supportController = new SupportController(controllerComponents)
}
