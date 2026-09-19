package controllers

import play.api.i18n.Messages
import play.api.libs.json.{JsObject, Json}

import java.time.format.TextStyle
import java.time.{DayOfWeek, Month}
import java.util.Locale

/**
 * The locale data `shared.js` needs at runtime that ISN'T a translated
 * string — the short + full weekday arrays for the date pills and the date
 * headers, the month array, and the showtime plural forms — serialised into
 * the `KINOWO_LOCALE` constant `_sharedJsConfig` emits. Translated UI strings
 * (empty-state / swipe-hint / clear / area-picker / auth-menu copy, and
 * everything else the templates pull from `messages(...)`) travel instead
 * via the universal language pack (`I18nPacks`, embedded alongside this) —
 * `shared.js` reads those by key directly rather than having them re-pulled
 * here, so there's exactly one place a key's text is sourced from.
 *
 * Polish stays byte-identical to the arrays shared.js used to inline (the
 * SHORT Java text for `pl` is "niedz."/"pon.", not the "Nie"/"Pon" the UI
 * wants, and the month labels are genitive — so PL keeps explicit arrays).
 * Other languages fall back to Java's locale-aware text.
 *
 * `KINOWO_LOCALE` carries this data for EVERY deployed language (under
 * `locales`), not just the deployment default — `i18n.js`'s `applyLanguage`
 * needs the visitor's OWN picked language's arrays to re-render the date
 * headers client-side, the same way it already swaps translated strings.
 * The top-level `day2`/`daysFull`/`months`/`plural`/`showtime` fields (the
 * deployment default) are kept alongside for the initial render.
 */
object JsLocale {

  // Sun-first (JS `Date.getDay()` is 0=Sunday) short weekday labels.
  private val PolishDays2  = Seq("Nie", "Pon", "Wto", "Śro", "Czw", "Pią", "Sob")
  // Full weekday names, Monday-first — matches `DateFormatter`'s
  // `LocalDate.getDayOfWeek.getValue - 1` indexing, which the date headers
  // (`.date-label`) it renders and the client-side re-render both key off.
  private val PolishDaysFull = Seq(
    "Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota", "Niedziela",
  )
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

  private def daysFull(locale: Locale): Seq[String] =
    if (isPolish(locale)) PolishDaysFull
    else DayOfWeek.values.toSeq.map(_.getDisplayName(TextStyle.FULL, locale).capitalize)

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

  /** The four languages the app ships — matches `play.i18n.langs`
   *  (`application.conf`), iOS's `LanguageSelection.supported`, and
   *  Android's `LocaleWrapper` equivalent. A visitor can pick any of these
   *  client-side regardless of the deployment's own default. */
  private val SupportedLanguages: Seq[String] = Seq("pl", "en", "de", "es")

  /** One language's full locale-derived payload — the deployment default's
   *  own fields (below) AND every entry under `locales` are built from this,
   *  so there's exactly one place that derives a language's day/month/plural
   *  data from its `Locale`. */
  private def localeData(locale: Locale): JsObject = {
    val (pluralRule, forms) = showtime(locale)
    Json.obj(
      "day2"     -> days2(locale),
      "daysFull" -> daysFull(locale),
      "months"   -> months(locale),
      "plural"   -> pluralRule,
      "showtime" -> forms,
    )
  }

  /** The `KINOWO_LOCALE` object literal (compact JSON): the deployment's
   *  language code, its own locale-derived data at the top level (for the
   *  initial render), and every deployed language's data under `locales`
   *  (for `i18n.js`'s `applyLanguage` to re-derive the date headers when the
   *  visitor picks a language other than the deployment default). */
  def json(messages: Messages): String = {
    val locales = SupportedLanguages.map(code => code -> localeData(Locale.forLanguageTag(code)))
    Json.stringify(
      Json.obj(
        "lang"    -> messages.lang.code,
        "locales" -> JsObject(locales),
      ) ++ localeData(messages.lang.toLocale)
    )
  }
}
