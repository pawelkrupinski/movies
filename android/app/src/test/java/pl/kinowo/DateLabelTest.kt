package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.model.DateLabel
import java.time.LocalDate
import java.util.Locale

/**
 * Regression for the day header staying in the DEPLOYMENT's default
 * language after a visitor picks a different in-app language
 * (`UserPreferences.selectedLanguageTag`, independent of the selected
 * country) — the server's `DayShowings.label` is baked once and never
 * reflects that pick, so `Showings.kt` derives the label from
 * `DateLabel.format` instead. Mirrors `DateFormatter.scala`'s output
 * byte-for-byte for the languages this app ships.
 */
class DateLabelTest {

    @Test
    fun polishUsesGenitiveMonthAndCapitalizedWeekday() {
        // 2026-06-04 is a Thursday.
        assertEquals("Czwartek 4 czerwca", DateLabel.format("2026-06-04", Locale.forLanguageTag("pl")))
    }

    @Test
    fun englishUsesItsOwnFullNames() {
        assertEquals("Thursday 4 June", DateLabel.format("2026-06-04", Locale.forLanguageTag("en")))
    }

    @Test
    fun germanUsesItsOwnFullNames() {
        assertEquals("Donnerstag 4 Juni", DateLabel.format("2026-06-04", Locale.forLanguageTag("de")))
    }

    @Test
    fun spanishUsesItsOwnFullNames() {
        // Weekday capitalized (matches DateFormatter.scala), month stays lowercase.
        assertEquals("Jueves 4 junio", DateLabel.format("2026-06-04", Locale.forLanguageTag("es")))
    }

    @Test
    fun appendsTheYearOnlyWhenItDiffersFromTheCurrentOne() {
        val currentYear = LocalDate.now().year
        val sameYearLabel = DateLabel.format("$currentYear-06-04", Locale.forLanguageTag("en"))
        assertFalse(sameYearLabel.contains(currentYear.toString()))

        val otherYear = currentYear + 1
        val otherYearLabel = DateLabel.format("$otherYear-06-04", Locale.forLanguageTag("en"))
        assertTrue(otherYearLabel.endsWith(" $otherYear"))
    }

    @Test
    fun theSameDateSwitchesLanguageWhenTheLocaleChanges() {
        // The exact bug this fixes: picking a different in-app language must
        // change this text, not leave it stuck in whatever the deployment (or
        // a previous pick) rendered.
        val polish = DateLabel.format("2026-06-04", Locale.forLanguageTag("pl"))
        val english = DateLabel.format("2026-06-04", Locale.forLanguageTag("en"))
        assertNotEquals(polish, english)
    }
}
