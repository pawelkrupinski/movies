package controllers

import play.api.i18n.Messages
import play.api.libs.json.{JsObject, Json}

import java.time.format.TextStyle
import java.time.{DayOfWeek, Month}
import java.util.Locale

/**
 * The locale data `shared.js` needs at runtime that ISN'T a translated
 * string — the short weekday + month arrays for the date pills and the
 * showtime plural forms — serialised into the `KINOWO_LOCALE` constant
 * `_sharedJsConfig` emits. Translated UI strings (empty-state / swipe-hint /
 * clear / area-picker / auth-menu copy, and everything else the templates
 * pull from `messages(...)`) travel instead via the universal language pack
 * (`I18nPacks`, embedded alongside this) — `shared.js` reads those by key
 * directly rather than having them re-pulled here, so there's exactly one
 * place a key's text is sourced from.
 *
 * Polish stays byte-identical to the arrays shared.js used to inline (the
 * SHORT Java text for `pl` is "niedz."/"pon.", not the "Nie"/"Pon" the UI
 * wants, and the month labels are genitive — so PL keeps explicit arrays).
 * Other languages fall back to Java's locale-aware text.
 */
object JsLocale {

  // Sun-first (JS `Date.getDay()` is 0=Sunday) short weekday labels.
  private val PolishDays2  = Seq("Nie", "Pon", "Wto", "Śro", "Czw", "Pią", "Sob")
  // Genitive month names ("5 czerwca"), matching the listing date headers.
  private val PolishMonths = Seq(
    "stycznia", "lutego", "marca", "kwietnia", "maja", "czerwca",
    "lipca", "sierpnia", "września", "października", "listopada", "grudnia",
  )

  private val SundayFirst: Seq[DayOfWeek] = Seq(
    DayOfWeek.SUNDAY, DayOfWeek.MONDAY, DayOfWeek.TUESDAY, DayOfWeek.WEDNESDAY,
    DayOfWeek.THURSDAY, DayOfWeek.FRIDAY, DayOfWeek.SATURDAY,
  )

  private def isPolish(locale: Locale): Boolean = locale.getLanguage == "pl"

  private def days2(locale: Locale): Seq[String] =
    if (isPolish(locale)) PolishDays2
    else SundayFirst.map(_.getDisplayName(TextStyle.SHORT, locale))

  private def months(locale: Locale): Seq[String] =
    if (isPolish(locale)) PolishMonths
    else Month.values.toSeq.map(_.getDisplayName(TextStyle.FULL, locale))

  /** Showtime plural forms + the plural RULE shared.js selects with. The rule id
   *  is not the language: `pl` is the three-form rule (seans / seanse / seansów)
   *  and `en` the two-form one/other rule, which German and Spanish share — the
   *  only branch `shared.js` takes is `=== 'pl'`. So a new language adds its
   *  WORD FORMS here and reuses whichever rule its grammar follows. */
  private def showtime(locale: Locale): (String, JsObject) = locale.getLanguage match {
    case "pl" => "pl" -> Json.obj("one" -> "seans", "few" -> "seanse", "many" -> "seansów")
    case "es" => "en" -> Json.obj("one" -> "sesión", "other" -> "sesiones")
    case "de" => "en" -> Json.obj("one" -> "Vorstellung", "other" -> "Vorstellungen")
    case _    => "en" -> Json.obj("one" -> "showing", "other" -> "showings")
  }

  /** The `KINOWO_LOCALE` object literal (compact JSON) for the given deployment
   *  messages: the deployment's language code, plus the locale-derived (not
   *  translated-string) data above. */
  def json(messages: Messages): String = {
    val locale               = messages.lang.toLocale
    val (pluralRule, forms)  = showtime(locale)
    Json.stringify(Json.obj(
      "lang"     -> messages.lang.code,
      "day2"     -> days2(locale),
      "months"   -> months(locale),
      "plural"   -> pluralRule,
      "showtime" -> forms,
    ))
  }
}
