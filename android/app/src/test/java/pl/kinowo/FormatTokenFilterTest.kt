package pl.kinowo

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test
import pl.kinowo.TestData.cinema
import pl.kinowo.TestData.slot
import pl.kinowo.filter.FormatTokenFilter
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import java.io.File

class FormatTokenFilterTest {

    private fun day(date: String, vararg cinemas: CinemaShowings) =
        DayShowings(date = date, label = date, cinemas = cinemas.toList())

    // ── the shared vectors ───────────────────────────────────────────────
    //
    // `test/resources/format-token-strip.json`, which the web's FilmFormatSpec
    // and iOS's FormatTokenFilterTests read too: the three platforms drop the
    // same tokens from a chip, case by case.

    @Serializable
    private data class Vectors(val dimensionTokens: Set<String>, val impliedDimension: String, val cases: List<Case>) {
        @Serializable
        data class Case(val name: String, val formats: List<String>, val strip: Set<String>)
    }

    /** Gradle runs unit tests from the module dir (`android/app`); walk up to the repo root. */
    private fun vectors(): Vectors {
        val start = File(System.getProperty("user.dir") ?: ".").absoluteFile
        val file = generateSequence(start) { it.parentFile }
            .map { it.resolve("test/resources/format-token-strip.json") }
            .firstOrNull { it.isFile }
            ?: error("test/resources/format-token-strip.json not found above $start")
        return Json { ignoreUnknownKeys = true }.decodeFromString(Vectors.serializer(), file.readText())
    }

    @Test
    fun theDimensionsAreTheSharedVectorsOnes() {
        val v = vectors()
        assertEquals(v.dimensionTokens, FormatTokenFilter.DIMENSION_TOKENS)
        assertEquals(v.impliedDimension, FormatTokenFilter.IMPLIED_DIMENSION)
    }

    @Test
    fun everyCaseOfTheSharedVectors() {
        val v = vectors()
        assertFalse(v.cases.isEmpty())
        for (c in v.cases) {
            val slots = c.formats.mapIndexed { i, f -> slot("1$i:00", f) }
            assertEquals(c.name, c.strip, FormatTokenFilter.tokensToStrip(listOf(day("2026-11-22", cinema("X", slots)))))
        }
    }

    @Test
    fun tokensToStripIsTheIntersectionAcrossShowtimes() {
        // Every slot is 2D, but language differs → only "2D" is common.
        val days = listOf(day("2026-09-05", cinema("X", listOf(
            slot("10:00", "2D NAP"), slot("12:00", "2D DUB"), slot("20:00", "2D NAP")))))
        assertEquals(setOf("2D"), FormatTokenFilter.tokensToStrip(days))
    }

    @Test
    fun tokensToStripKeepsAllWhenEveryShowtimeMatches() {
        val days = listOf(day("2026-09-05", cinema("X", listOf(slot("10:00", "2D NAP"), slot("12:00", "2D NAP")))))
        assertEquals(setOf("2D", "NAP"), FormatTokenFilter.tokensToStrip(days))
    }

    @Test
    fun tokensToStripEmptyWhenNoSharedToken() {
        val days = listOf(day("2026-09-05", cinema("X", listOf(slot("10:00", "2D NAP"), slot("12:00", "3D DUB")))))
        assertEquals(emptySet<String>(), FormatTokenFilter.tokensToStrip(days))
    }

    @Test
    fun filterRemovesTheCommonTokens() {
        // "2D" is redundant when every slot is 2D — drop it from the per-badge label.
        assertEquals("NAP", FormatTokenFilter.filter("2D NAP", setOf("2D")))
        assertEquals("IMAX 3D", FormatTokenFilter.filter("IMAX 3D DUB", setOf("DUB")))
    }

    @Test
    fun filterIsANoOpWhenNothingIsCommon() {
        assertEquals("2D NAP", FormatTokenFilter.filter("2D NAP", emptySet()))
    }

    // ── what a chip is left showing ──────────────────────────────────────
    //
    // The whole rendering decision: what a chip drops, and therefore what is
    // left to read. `UniformFormatChipTest` renders it; these pin the rule.

    @Test
    fun aVersionTheWholeCardSharesIsDroppedLikeAnyOtherToken() {
        val days = listOf(day("2026-09-05", cinema("Multikino", listOf(slot("14:30", "2D DUB"), slot("17:00", "2D DUB")))))
        val common = FormatTokenFilter.tokensToStrip(days)
        assertEquals(setOf("2D", "DUB"), common)
        // Six chips all saying DUB tell a visitor as little as six all saying
        // 2D — the film screens no other way here.
        assertEquals("", FormatTokenFilter.filter("2D DUB", common))
    }

    @Test
    fun twoCinemasThatDisagreeKeepTheVersionOnBoth() {
        // Neither cinema is mixed on its own; the FILM is. That difference is
        // the whole reason a visitor reads the tag, so it stays on every chip.
        val days = listOf(day("2026-09-05",
            cinema("Multikino", listOf(slot("14:30", "2D DUB"), slot("17:00", "2D DUB"))),
            cinema("Helios",    listOf(slot("15:00", "2D NAP"), slot("19:00", "2D NAP")))))
        val common = FormatTokenFilter.tokensToStrip(days)
        assertEquals(setOf("2D"), common)
        assertEquals("DUB", FormatTokenFilter.filter("2D DUB", common))
        assertEquals("NAP", FormatTokenFilter.filter("2D NAP", common))
    }

    @Test
    fun twoDaysThatDisagreeKeepTheVersionOnBoth() {
        // Same cinema, subtitled today and dubbed tomorrow: uniform within each
        // day, mixed across the card.
        val days = listOf(
            day("2026-09-05", cinema("Multikino", listOf(slot("14:30", "2D NAP"), slot("17:00", "2D NAP")))),
            day("2026-09-06", cinema("Multikino", listOf(slot("14:30", "2D DUB"), slot("17:00", "2D DUB")))))
        val common = FormatTokenFilter.tokensToStrip(days)
        assertEquals(setOf("2D"), common)
        assertEquals("NAP", FormatTokenFilter.filter("2D NAP", common))
        assertEquals("DUB", FormatTokenFilter.filter("2D DUB", common))
    }

    @Test
    fun aSharedScreenFormatIsDropped() {
        val days = listOf(day("2026-09-05", cinema("X", listOf(slot("14:30", "IMAX NAP"), slot("17:00", "IMAX 3D NAP")))))
        assertEquals(setOf("IMAX", "NAP"), FormatTokenFilter.tokensToStrip(days))
        assertEquals("3D", FormatTokenFilter.filter("IMAX 3D NAP", setOf("IMAX", "NAP")))
    }

    @Test
    fun aVersionThatDiffersBetweenSlotsStaysOnEveryChip() {
        val days = listOf(day("2026-09-05", cinema("Multikino", listOf(slot("14:30", "2D NAP"), slot("17:00", "2D DUB")))))
        val common = FormatTokenFilter.tokensToStrip(days)
        assertEquals("NAP", FormatTokenFilter.filter("2D NAP", common))
        assertEquals("DUB", FormatTokenFilter.filter("2D DUB", common))
    }
}
