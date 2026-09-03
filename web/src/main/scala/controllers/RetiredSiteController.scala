package controllers

import models.Country
import play.api.mvc._

/**
 * Everything a RETIRED deployment serves. One country's site has moved to
 * another host; this process keeps answering on the old one so published links,
 * bookmarks and installed apps do not simply go dark.
 *
 * Three behaviours, and the split is deliberate:
 *
 *   - `/` and `/{city}/` render a NOTICE — the address changed, here is the new
 *     one. A person who typed the old host or followed an old bookmark is told
 *     so, rather than silently landing somewhere else and never updating it.
 *   - EVERYTHING ELSE redirects permanently ([[RetiredSite.redirectStatus]]).
 *     Deep links (a film page, every `/api/…` the mobile apps call)
 *     have no reader to inform — they have a client that wants the resource, and
 *     a redirect is what gets it there. It is also what keeps SHARE PREVIEWS
 *     intact: Facebook, Slack, WhatsApp and Telegram all follow a 30x when they
 *     scrape, so a film link shared years ago still previews off the live page.
 *   - `/health` answers, because the platform's health check decides whether
 *     this process is allowed to keep running at all.
 *
 * THE NOTICE PAGES CARRY THE LIVE SITE'S OWN METADATA, not a description of
 * themselves: the same `<title>`, Open Graph title/description/image and
 * canonical URL the real page emits, all of which are derived from the city and
 * the country rather than from the repertoire, so they need no database behind
 * them. A link to the old host therefore previews exactly as it always did, and
 * the canonical hands the indexing to the live site instead of competing with
 * it.
 */
class RetiredSiteController(cc: ControllerComponents, country: Country)(implicit messages: play.api.i18n.Messages)
    extends AbstractController(cc) {

  /** Scheme + host of the live site, WITHOUT the mount prefix — see
   *  [[RetiredSite.destination]] for why the prefixed `webUrl` is the wrong base
   *  to append an incoming path to. A country with nowhere to point at cannot be
   *  retired, and failing here fails the boot rather than every request. */
  private val liveOrigin: String = country.webOrigin.getOrElse(throw new IllegalStateException(
    s"KINOWO_RETIRED is set but ${country.code} has no webOrigin — there is no live site to send visitors to"))

  def landing: Action[AnyContent] = Action {
    notice(
      pageTitle       = messages("landing.title", country.brandName),
      pageDescription = messages("landing.ogDescription"),
      pageUrl         = country.ogOrigin + "/",
      imageUrl        = country.ogOrigin + "/assets/img/" + country.homeOgImage)
  }

  /** A city this deployment serves gets the notice; anything else shaped like
   *  `/{segment}/` is not a city page and is treated like any other deep link. */
  def city(slug: String): Action[AnyContent] = Action { request =>
    country.bySlug.get(slug) match {
      case Some(city) => notice(
        pageTitle       = FilterDescription.defaultTitle(city),
        pageDescription = FilterDescription.defaultDescription(city),
        pageUrl         = liveOrigin + CityPath(city) + "/",
        imageUrl        = s"${country.ogOrigin}/assets/img/${city.shareImage}")
      case None => toLiveSite(request)
    }
  }

  def elsewhere: Action[AnyContent] = Action(toLiveSite)

  def health: Action[AnyContent] = Action(Ok("retired"))

  /** Nothing scrapes a retired host, and a REDIRECTED `/metrics` would be worse
   *  than none: a stale Prometheus job would start filing the live site's series
   *  under this host's labels. */
  def metrics: Action[AnyContent] = Action(NotFound)

  /** `pageUrl` is both the canonical/`og:url` of the page this one replaces AND
   *  where the visitor is sent — they are the same address by construction, so
   *  the notice cannot advertise one destination and link to another. */
  private def notice(pageTitle: String, pageDescription: String, pageUrl: String, imageUrl: String): Result =
    Ok(views.html.moved(
      pageTitle        = pageTitle,
      pageDescription  = pageDescription,
      pageUrl          = pageUrl,
      imageUrl         = imageUrl,
      destinationLabel = Country.withoutScheme(pageUrl).stripSuffix("/")))

  private def toLiveSite(request: RequestHeader): Result =
    Redirect(
      RetiredSite.destination(liveOrigin, request.path, request.rawQueryString),
      Map.empty[String, Seq[String]],
      RetiredSite.redirectStatus(request.method))
}
