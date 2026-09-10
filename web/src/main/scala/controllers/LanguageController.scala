package controllers

import play.api.i18n.Lang
import play.api.mvc._
import play.api.mvc.Results.ResultWithLang

/** `/lang/:code` — the one write path for a visitor's explicit language pick,
 *  reached from the navbar's language `<select>` (`onLanguageChange` in
 *  `shared.js`). Sets Play's own `PLAY_LANG` cookie via `Result.withLang` (no
 *  bespoke cookie code) and bounces back to wherever the visitor was.
 *
 *  A full navigation (`GET`, not a fetch), matching `onCountryChange` /
 *  `onCityChange`: the whole page has to re-render in the new language
 *  anyway, so there is nothing a view-swap would save. */
class LanguageController(cc: ControllerComponents) extends AbstractController(cc) {

  def set(code: String, back: String): Action[AnyContent] = Action {
    val target = LanguageController.safeBack(back)
    WebLangResolver.Supported.find(_ == code) match {
      case Some(supported) => Redirect(target).withLang(Lang(supported))
      case None             => Redirect(target)
    }
  }
}

object LanguageController {

  /** `back` is a relative path the visitor was on, not a credential-carrying
   *  redirect (contrast `AuthController.switchTarget`'s host allowlist), so a
   *  same-origin check is enough: it must start with a single `/` and not the
   *  `//host/...` spelling a browser would treat as protocol-relative. Any
   *  other shape (an absolute URL, an empty string) falls back to the site
   *  root rather than being trusted. */
  def safeBack(back: String): String =
    if (back.startsWith("/") && !back.startsWith("//")) back else "/"
}
