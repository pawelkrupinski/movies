package pl.kinowo.ui.common

import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.width
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.Modifier
import androidx.compose.ui.test.getUnclippedBoundsInRoot
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.filter.CinemaSection
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Showtime
import pl.kinowo.ui.detail.DetailScreen
import pl.kinowo.ui.list.CinemaGrid
import pl.kinowo.ui.theme.KinowoTheme

/**
 * Off-device (Robolectric) Compose layout tests pinning that the two new tuning
 * styles — [CinemaHeaderStyle] (Kina page) and [FilmDetailStyle] (Film page) —
 * actually drive their real views. The tuning pager would be a no-op preview if
 * the cinema-header font / detail-header font levers weren't wired through to
 * `CinemaGrid` / `DetailScreen`; these render the real composables with a big
 * vs default override and assert the rendered text height moves.
 *
 * Before the styles existed, those sizes were hard-coded, so a style override
 * changed nothing → the two heights would match → fail. NATIVE graphics for real
 * metrics; `xhdpi` so sub-dp differences resolve. Runs on the JVM via
 * `./gradlew app:testDebugUnitTest` — no emulator. Sibling of [CardSpacingStyleTest].
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "xhdpi")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class TuningStylesTest {

    @get:Rule
    val compose = createComposeRule()

    /** Heights (dp) of every rendered node carrying [text], in tree order. */
    private fun textHeights(text: String): List<Float> {
        val nodes = compose.onAllNodesWithText(text, useUnmergedTree = true)
        val count = nodes.fetchSemanticsNodes().size
        return (0 until count).map {
            val b = nodes[it].getUnclippedBoundsInRoot()
            (b.bottom - b.top).value
        }
    }

    private fun sectionFor(cinema: String) = listOf(
        CinemaSection(
            cinema = cinema,
            films = listOf(
                Film(
                    title = "T-$cinema",
                    showings = listOf(
                        DayShowings(
                            date = "2026-06-08", label = "Poniedziałek",
                            cinemas = listOf(CinemaShowings(cinema = cinema, showtimes = listOf(Showtime(time = "12:55", format = "2D")))),
                        ),
                    ),
                ),
            ),
        ),
    )

    private fun filmWithShowings(title: String) = Film(
        title = title,
        directors = listOf("Reżyser"),
        showings = listOf(
            DayShowings(
                date = "2026-06-08", label = "Poniedziałek",
                cinemas = listOf(CinemaShowings(cinema = "Kino", showtimes = listOf(Showtime(time = "12:55", format = "2D")))),
            ),
        ),
    )

    /**
     * Two `CinemaGrid`s with the SAME cinema name, one driven by a 24sp header
     * font and one by the 15sp default. The 24sp section header must render
     * taller than the default — only true if [LocalCinemaHeaderStyle]'s font
     * feeds the header. Hard-code the size and the two heights match → fail.
     */
    @Test
    fun cinemaHeaderFontSizeWidensTheHeader() {
        compose.setContent {
            KinowoTheme {
                Row {
                    Box(Modifier.width(180.dp)) {
                        CompositionLocalProvider(LocalCinemaHeaderStyle provides CinemaHeaderStyle(fontSize = 24.sp)) {
                            CinemaGrid(sectionFor("KinoX"), showHeaders = true, bottomInset = 0.dp, scrollResetKey = null, onOpen = {}, onHide = {})
                        }
                    }
                    Box(Modifier.width(180.dp)) {
                        CompositionLocalProvider(LocalCinemaHeaderStyle provides CinemaHeaderStyle()) {
                            CinemaGrid(sectionFor("KinoX"), showHeaders = true, bottomInset = 0.dp, scrollResetKey = null, onOpen = {}, onHide = {})
                        }
                    }
                }
            }
        }

        val heights = textHeights("KinoX")
        assertTrue("expected two cinema headers, got ${heights.size}", heights.size >= 2)
        assertTrue("header measured no height — metrics are stubbed", heights.min() > 0f)
        assertTrue(
            "24sp cinema header must be taller than the 15sp default: $heights",
            heights.max() - heights.min() > 3f,
        )
    }

    /**
     * Two `DetailScreen`s, one driven by a 26sp "Seanse" header and one by the
     * 14sp default. The big header must render taller — only true if
     * [LocalFilmDetailStyle]'s showings-header font feeds the screen. `details =
     * null` keeps the trailer WebView out of the composition.
     */
    @Test
    fun filmDetailShowingsHeaderFontIsTunable() {
        compose.setContent {
            KinowoTheme {
                Column {
                    Box(Modifier.height(420.dp).fillMaxWidth()) {
                        CompositionLocalProvider(LocalFilmDetailStyle provides FilmDetailStyle(showingsHeaderFontSize = 26.sp)) {
                            DetailScreen(film = filmWithShowings("AAA"), details = null, onBack = {})
                        }
                    }
                    Box(Modifier.height(420.dp).fillMaxWidth()) {
                        CompositionLocalProvider(LocalFilmDetailStyle provides FilmDetailStyle()) {
                            DetailScreen(film = filmWithShowings("BBB"), details = null, onBack = {})
                        }
                    }
                }
            }
        }

        val heights = textHeights("Seanse")
        assertTrue("expected two Seanse headers, got ${heights.size}", heights.size >= 2)
        assertTrue("Seanse header measured no height — metrics are stubbed", heights.min() > 0f)
        assertTrue(
            "26sp Seanse header must exceed the 14sp default: $heights",
            heights.max() - heights.min() > 3f,
        )
    }

    /** The defaults must equal the literals the views used before the styles
     *  existed, so production (which never overrides the locals) renders
     *  unchanged. Tighten/loosen a real literal and this catches the drift. */
    @Test
    fun defaultsMatchShippingLiterals() {
        val h = CinemaHeaderStyle()
        assertEquals(15f, h.fontSize.value, 0.01f)
        assertEquals(FontWeight.SemiBold, h.fontWeight)
        assertEquals(8f, h.headerTopGap.value, 0.01f)
        assertEquals(2f, h.headerToGrid.value, 0.01f)
        assertEquals(12f, h.sectionSpacing.value, 0.01f)

        val f = FilmDetailStyle()
        assertEquals(20f, f.titleFontSize.value, 0.01f)
        assertEquals(FontWeight.Bold, f.titleWeight)
        assertEquals(14f, f.originalTitleFontSize.value, 0.01f)
        assertEquals(11f, f.sectionLabelFontSize.value, 0.01f)
        assertEquals(14f, f.sectionBodyFontSize.value, 0.01f)
        assertEquals(14f, f.showingsHeaderFontSize.value, 0.01f)
        assertEquals(16f, f.outerSpacing.value, 0.01f)
        assertEquals(8f, f.headerSpacing.value, 0.01f)
    }
}
