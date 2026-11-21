package modules.webwiring

import controllers.{CatalogController, ClientSupportController, HealthController, LandingController, LegalController, PickerController, PickerFragments, SupportController, WellKnownController}

/** ── The site around the cities ────────────────────────────────────────────
 *  The pages a visitor (or the mobile apps) reach outside any one city, without signing in: the landing page and its
 *  country picker, the app catalog, the legal/support pages and the operational `/health` + `.well-known` answers;
 *  and the deployment's language every visitor-facing render uses. The city pages are [[CityPagesWiring]]'s. */
trait SiteWiring { self: Wiring =>

  // The deployment's DEFAULT, and ONLY, `Lang` — Poland → pl → default
  // `messages`; other countries → their own bundle. `LandingController` /
  // `MovieController` render every visitor in this; an explicit language
  // pick swaps the visible copy client-side instead (`js/shared/`'s
  // `applyLanguage`), never by re-rendering server-side.
  lazy val deploymentLang: play.api.i18n.Lang = play.api.i18n.Lang(country.language)

  // The fixed deployment `Messages` — what every visitor-facing render uses,
  // plus the `DebugController`-adjacent, ops/crawler-facing ones.
  implicit lazy val deploymentMessages: play.api.i18n.Messages =
    messagesApi.preferred(Seq(deploymentLang))

  // Every country's `/` picker fragment, rendered once for the life of the process:
  // the landing embeds its own country's, `/api/picker/:country` serves the rest.
  lazy val pickerFragments = PickerFragments.rendered()
  lazy val landingController = new LandingController(controllerComponents, country, pickerFragments)
  lazy val pickerController  = new PickerController(controllerComponents, pickerFragments)
  // Global country+city catalog for the mobile apps (`GET /api/catalog`), served
  // identically by every deployment — no per-country/read-model dependency.
  lazy val catalogController = new CatalogController(controllerComponents)
  lazy val clientSupportController = new ClientSupportController(controllerComponents)
  lazy val healthController = new HealthController(controllerComponents, readiness = () => webReadModel.hydrated,
    databases = Seq(mongoConnection, usersConnection))
  lazy val wellKnownController = new WellKnownController(controllerComponents)
  lazy val legalController   = new LegalController(controllerComponents, country)
  lazy val supportController = new SupportController(controllerComponents, country)

  /** The controllers the router serves the site's routes with ([[modules.AppComponents.router]]). */
  lazy val siteRouted = (landingController, pickerController, catalogController, clientSupportController, healthController,
    wellKnownController, legalController, supportController)
}
