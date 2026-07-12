package controllers

import play.api.i18n.Messages
import play.api.libs.json.{JsObject, Json}

import java.time.format.TextStyle
import java.time.{DayOfWeek, Month}
import java.util.Locale

/**
 * The locale data `shared.js` needs at runtime, serialised into the
 * `KINOWO_LOCALE` constant `_sharedJsConfig` emits. Everything the client-side
 * JS used to hardcode in Polish — the short weekday + month arrays for the
 * date pills, the empty-state / swipe-hint / clear strings, and the showtime
 * plural forms — is derived here from the deployment's language so an English
 * deployment renders English JS output.
 *
 * Polish stays byte-identical to the arrays/strings shared.js used to inline
 * (the SHORT Java text for `pl` is "niedz."/"pon.", not the "Nie"/"Pon" the UI
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

  /** Showtime plural forms + the plural rule shared.js selects with. Polish has
   *  three forms (seans / seanse / seansów); English two (showing / showings). */
  private def showtime(locale: Locale): (String, JsObject) =
    if (isPolish(locale))
      "pl" -> Json.obj("one" -> "seans", "few" -> "seanse", "many" -> "seansów")
    else
      "en" -> Json.obj("one" -> "showing", "other" -> "showings")

  /** The `KINOWO_LOCALE` object literal (compact JSON) for the given deployment
   *  messages. Scalar UI strings come from the message bundle so they stay in
   *  lock-step with the server-rendered copy. */
  def json(messages: Messages): String = {
    val locale               = messages.lang.toLocale
    val (pluralRule, forms)  = showtime(locale)
    Json.stringify(Json.obj(
      "lang"            -> messages.lang.code,
      "day2"            -> days2(locale),
      "months"          -> months(locale),
      "emptyRepertoire" -> messages("empty.repertoire"),
      "swipeHint"       -> messages("hint.swipe"),
      "clear"           -> messages("nav.clear"),
      // Strings for the first-visit area picker on a split city (built in JS).
      "areaPicker"      -> Json.obj(
        "title"    -> messages("areaPicker.title"),
        "subtitle" -> messages("areaPicker.subtitle"),
        "confirm"  -> messages("areaPicker.confirm"),
      ),
      "plural"          -> pluralRule,
      "showtime"        -> forms,
    ))
  }
}
