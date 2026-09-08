package controllers

import models.{ClientSupport, Country}
import play.api.mvc._

/**
 * Everything a RETIRED deployment serves. One country's site has moved to
 * another host; this process keeps answering on the old one so published links,
 * bookmarks and installed apps do not simply go dark.
 *
 * Four behaviours, and the split is deliberate:
 *
 *   - `/` and `/{city}/` render a NOTICE — the address changed, here is the new
 *     one. A person who typed the old host or followed an old bookmark is told
 *     so, rather than silently landing somewhere else and never updating it.
 *   - ANY OTHER `GET`/`HEAD` that isn't `/api/…` or a machine file
 *     ([[RetiredSite.isMachineFile]]) — a film page, `/plan`, a legal page —
 *     renders the SAME notice. There is no per-page title or image to give it
 *     (a retired deployment has no database, see [[modules.RetiredComponents]]),
 *     so it carries the brand-level one `/` does, but the link itself still goes
 *     to the real page. This trades away rich per-page SHARE PREVIEWS for an old
 *     film link (Facebook/Slack/WhatsApp/Telegram unfurl the generic notice
 *     rather than that film's real title and poster) in exchange for a person
 *     who lands on it directly being told what happened, not silently bounced.
 *   - `/api/…` gets told to UPGRADE ([[apiUpgradeRequired]]) instead of being
 *     handed the live API's current response — a client old enough to still be
 *     pointed at this host may be old enough that the live site's current JSON
 *     shape has drifted past what it can parse.
 *   - EVERYTHING ELSE — writes (`PUT`/`POST`/`DELETE`) outside `/api/…` like
 *     `/auth/token`, and the machine files — redirects permanently
 *     ([[RetiredSite.redirectStatus]]), unchanged from before: a program wants
 *     the resource, not a page to read.
 *   - `/health` answers, because the platform's health check decides whether
 *     this process is allowed to keep running at all.
 *
 * THE NOTICE PAGES CARRY THE LIVE SITE'S OWN METADATA where there is any to
 * carry: `/` and `/{city}/` pass the same `<title>`, Open Graph title/
 * description/image and canonical URL the real page emits, derived from the
 * city and the country rather than from the repertoire, so they need no
 * database behind them. A link to one of those two therefore previews exactly
 * as it always did; every other notice falls back to the brand-level one.
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

  /** The catch-all — see the class doc for the four-way split this makes. */
  def elsewhere: Action[AnyContent] = Action { request =>
    if (RetiredSite.isApiPath(request.path))
      apiUpgradeRequired
    else if ((request.method == "GET" || request.method == "HEAD") && !RetiredSite.isMachineFile(request.path))
      genericNotice(request)
    else
      toLiveSite(request)
  }

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

  /** Any other page a person might land on — a film, `/plan`, a legal page. No
   *  per-page title/image exists to give it (see the class doc), so it carries
   *  the same brand-level notice `landing` does; only `pageUrl` is specific to
   *  the page that was actually asked for. */
  private def genericNotice(request: RequestHeader): Result =
    notice(
      pageTitle       = messages("landing.title", country.brandName),
      pageDescription = messages("landing.ogDescription"),
      pageUrl         = RetiredSite.destination(liveOrigin, request.path, request.rawQueryString),
      imageUrl        = s"${country.ogOrigin}/assets/img/${country.homeOgImage}")

  /** `/api/…` on a retired host: told to upgrade rather than transparently
   *  handed whatever the live API now returns. Same payload `GET
   *  /api/client-support` already serves on the live site — no new contract for
   *  a future client to learn — but 426, not 200: a status a client checks for
   *  is a status it can act on, where a 200 with an unfamiliar body looks like
   *  success. Neither app reads either signal today; this is here for the day
   *  one does. */
  private def apiUpgradeRequired: Result =
    Status(play.api.http.Status.UPGRADE_REQUIRED)(ClientSupport.json)
      .as("application/json")
      .withHeaders("ETag" -> ClientSupport.etag, "Cache-Control" -> "no-cache")

  private def toLiveSite(request: RequestHeader): Result =
    Redirect(
      RetiredSite.destination(liveOrigin, request.path, request.rawQueryString),
      Map.empty[String, Seq[String]],
      RetiredSite.redirectStatus(request.method))
}
