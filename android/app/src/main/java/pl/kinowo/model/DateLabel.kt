package pl.kinowo.model

import java.time.LocalDate
import java.time.format.TextStyle
import java.util.Locale

/**
 * Formats a [DayShowings.date] (`YYYY-MM-DD`) into the long weekday + day +
 * month label the web renders for the same date (`DateFormatter.format` in
 * `web/src/main/scala/controllers/DateFormatter.scala`), e.g. "Czwartek 4
 * czerwca" / "Thursday 4 June" — in the CURRENT locale, never
 * [DayShowings.label].
 *
 * [DayShowings.label] used to be shown directly, but it's rendered ONCE by
 * the server in the deployment's fixed default language and stays that way
 * regardless of the visitor's own in-app language pick
 * (`UserPreferences.selectedLanguageTag`, independent of the selected
 * country) — `recreate()` re-applies the picked locale to every OTHER string
 * on screen (`LocaleWrapper` forces `Locale.getDefault()`), but the already-
 * fetched `label` field doesn't follow it. Deriving it here from the raw
 * `date`, against [Locale.getDefault] at call time, fixes that the same way
 * the web's client-side `formatDateLabel` (`shared.js`) does.
 */
object DateLabel {
    private val polishDays = listOf(
        "poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela",
    )
    private val polishMonths = listOf(
        "", "stycznia", "lutego", "marca", "kwietnia", "maja", "czerwca",
        "lipca", "sierpnia", "września", "października", "listopada", "grudnia",
    )

    /** [isoDate] is `YYYY-MM-DD`. Returns [isoDate] itself if it doesn't parse
     *  (defensive only — every `date` this app decodes comes from the API's
     *  own ISO-formatted field). */
    fun format(isoDate: String, locale: Locale = Locale.getDefault()): String {
        val date = runCatching { LocalDate.parse(isoDate) }.getOrNull() ?: return isoDate
        val currentYear = LocalDate.now().year
        val yearSuffix = if (date.year == currentYear) "" else " ${date.year}"
        val dayName = if (locale.language == "pl") {
            polishDays[date.dayOfWeek.value - 1].replaceFirstChar { it.uppercase() }
        } else {
            date.dayOfWeek.getDisplayName(TextStyle.FULL, locale).replaceFirstChar { it.uppercase() }
        }
        val monthName = if (locale.language == "pl") {
            polishMonths[date.monthValue]
        } else {
            date.month.getDisplayName(TextStyle.FULL, locale)
        }
        return "$dayName ${date.dayOfMonth} $monthName$yearSuffix"
    }
}
