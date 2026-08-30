package modules

import controllers.Assets.Asset
import controllers.{AssetsComponents, RetiredSiteController, WellKnownController}
import models.Country
import play.api.ApplicationLoader.Context
import play.api._
import play.api.routing.Router
import play.filters.HttpFiltersComponents

/**
 * The composition root a RETIRED deployment boots instead of [[AppComponents]]
 * — chosen once at startup by `KINOWO_RETIRED` (see [[AppLoader.load]]).
 *
 * IT IS A SEPARATE CLASS RATHER THAN A FLAG INSIDE THE REAL ONE, and that is
 * the whole point of it. A retired host has no repertoire to serve: keeping the
 * data graph and merely refusing to render it would still open a Mongo pool, a
 * change stream on `web_movies`/`web_screenings` and the users database, so a
 * host nobody visits would go on paying for — and putting load on — the live
 * site's database forever. Not mixing in `Wiring` makes that structurally
 * impossible instead of a discipline someone has to keep.
 *
 * What survives is what answers without data: the notice pages, the redirects,
 * the static `/assets` (the notice's own favicon), and the app-association files
 * under `/.well-known/` — an app installed before the move still has the old
 * host in its Universal Links, and dropping those would send it to Safari.
 */
class RetiredComponents(context: Context, country: Country)
    extends BuiltInComponentsFromContext(context)
    with HttpFiltersComponents
    with AssetsComponents {

  // The one `Messages` this deployment renders with, fixed at boot from the
  // country — same rule as `Wiring.deploymentMessages`; a retired site speaks
  // the language of the site it replaced.
  private implicit lazy val deploymentMessages: play.api.i18n.Messages =
    messagesApi.preferred(Seq(play.api.i18n.Lang(country.language)))

  lazy val retiredSiteController = new RetiredSiteController(controllerComponents, country)
  lazy val wellKnownController   = new WellKnownController(controllerComponents)

  // No generated `Routes` is instantiated here, and instantiating it is what
  // normally publishes the mount point to `router.RoutesPrefix` — which the
  // asset reverse route in `_favicon` reads. Publish it directly, or the notice
  // page on a prefixed country would link its favicon one segment too high.
  _root_.router.RoutesPrefix.setPrefix(httpConfiguration.context)

  lazy val router: Router =
    AppLoader.rootOperationalRoutes(retiredSiteController.health, retiredSiteController.metrics)
      .orElse(AppLoader.retiredRoutes(
        retiredSiteController,
        wellKnownController,
        file => assets.versioned("/public", Asset(file))
      ).withPrefix(httpConfiguration.context))
}
