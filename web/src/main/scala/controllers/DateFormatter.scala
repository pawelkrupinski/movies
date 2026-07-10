package controllers

import java.time.LocalDate
import java.time.format.TextStyle
import java.util.Locale

/** Long date labels for the listing date headers / JSON day labels / OG cards.
 *
 *  Polish keeps its explicit genitive month names ("Czwartek 4 czerwca") — Java's
 *  locale text gives the nominative ("czerwiec"), which reads wrong in a date —
 *  and stays byte-identical to what the templates rendered before i18n. Other
 *  languages fall back to Java's locale-aware full weekday + month names
 *  ("Thursday 4 June"). The deployment's language is resolved once from the
 *  country ([[models.Country.fromEnv]]); callers that already hold a locale can
 *  pass it explicitly (tests). */
object DateFormatter {

  private val polishDays = Vector(
    "poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"
  )

  private val polishMonths = Vector(
    "", "stycznia", "lutego", "marca", "kwietnia", "maja", "czerwca",
    "lipca", "sierpnia", "września", "października", "listopada", "grudnia"
  )

  def format(date: LocalDate): String = format(date, models.Country.fromEnv.language)

  def format(date: LocalDate, locale: Locale): String = {
    val currentYear = LocalDate.now().getYear
    val yearSuffix  = if (date.getYear == currentYear) "" else s" ${date.getYear}"
    val dayName     =
      if (locale.getLanguage == "pl") polishDays(date.getDayOfWeek.getValue - 1).capitalize
      else date.getDayOfWeek.getDisplayName(TextStyle.FULL, locale).capitalize
    val monthName   =
      if (locale.getLanguage == "pl") polishMonths(date.getMonthValue)
      else date.getMonth.getDisplayName(TextStyle.FULL, locale)
    s"$dayName ${date.getDayOfMonth} $monthName$yearSuffix"
  }
}
