package controllers

import play.api.i18n.Lang
import play.api.mvc.RequestHeader

/** Resolves the `Lang` a single request renders in — independent of, and
 *  layered above, the deployment's fixed default.
 *
 *  Same shape as the mobile apps' language resolution (see `LanguageSelection`
 *  on iOS, `LanguageDefault`/`RegionLanguage` on Android): an explicit pick
 *  first, then the visitor's own stated preference, then a region-based
 *  fallback — here, simply the deployment's own country, since a web
 *  deployment already serves exactly one country/domain and has no broader
 *  "store region" concept to fall back through. */
object WebLangResolver {

  /** The four languages every deployment ships bundles for
   *  (`play.i18n.langs` in `application.conf`). */
  val Supported: Set[String] = Set("pl", "en", "de", "es")

  /** The `Lang` this request renders in: the `PLAY_LANG` cookie (an explicit
   *  pick, made via `LanguageController.set`) if it names a supported
   *  language, else the first supported language in `Accept-Language`, else
   *  `deploymentDefault`. */
  def resolve(request: RequestHeader, deploymentDefault: Lang): Lang =
    cookieLang(request)
      .orElse(acceptLanguage(request))
      .map(Lang(_))
      .getOrElse(deploymentDefault)

  private def cookieLang(request: RequestHeader): Option[String] =
    request.cookies.get(Play2CookieName).map(_.value).filter(Supported.contains)

  private def acceptLanguage(request: RequestHeader): Option[String] =
    request.acceptLanguages.map(_.language).find(Supported.contains)

  // Play's own `Lang` cookie name — `LanguageController.set` writes it via
  // `Result.withLang`, which honours `play.i18n.langCookieName` (unset here,
  // so it's Play's default). Named explicitly rather than read off
  // `messagesApi.langCookieName` so this stays a pure function of the request.
  private val Play2CookieName = "PLAY_LANG"
}
