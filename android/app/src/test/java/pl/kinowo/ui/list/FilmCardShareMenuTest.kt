package pl.kinowo.ui.list

import android.content.ClipboardManager
import android.content.Context
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performTouchInput
import androidx.compose.ui.test.longClick
import androidx.test.core.app.ApplicationProvider
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.TestData
import pl.kinowo.model.Film
import pl.kinowo.model.Showtime
import pl.kinowo.ui.common.LocalCitySlug
import pl.kinowo.ui.common.ShowtimeChipTestTag
import pl.kinowo.ui.common.filmShareUrl

/**
 * Long-pressing a [FilmCard]'s poster opens a share menu — Udostępnij (system
 * share sheet) and Skopiuj link (clipboard) — mirroring iOS `FilmCardView`'s
 * `.contextMenu`. The menu lives on the poster, NOT the whole card, so it
 * doesn't swallow the showtime chips' own long-press (the room tooltip); a
 * plain tap anywhere still opens the detail screen and shows no menu.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "pl")
class FilmCardShareMenuTest {

    @get:Rule
    val compose = createComposeRule()

    private val film = TestData.film(title = "Diuna: Część druga", days = emptyList())

    /** A card carrying one room-bearing showtime, for the chip long-press test. */
    private val filmWithRoom = TestData.film(
        title = "Diuna: Część druga",
        days = listOf(
            TestData.day(
                "Dziś",
                listOf(
                    TestData.cinema(
                        "Kino Pod Baranami",
                        listOf(Showtime(time = "18:30", format = "2D NAP", room = "Sala 8")),
                    ),
                ),
            ),
        ),
    )

    private fun render(film: Film = this.film, onOpen: () -> Unit = {}) {
        compose.setContent {
            // Mirror the production wiring: the city slug is provided once at the
            // city-gate root (KinowoApp), so the share link is city-scoped.
            CompositionLocalProvider(LocalCitySlug provides "poznan") {
                FilmCard(film = film, showCinemaHeaders = true, onOpen = onOpen, onHide = {})
            }
        }
    }

    @Test
    fun longPressOnPosterOpensShareMenu() {
        var opened = false
        render(onOpen = { opened = true })

        compose.onNodeWithTag(FilmCardPosterTestTag).performTouchInput { longClick() }
        compose.waitForIdle()

        compose.onNodeWithText("Udostępnij").assertExists()
        compose.onNodeWithText("Skopiuj link").assertExists()
        assertFalse("long-press must not open the detail screen", opened)
    }

    /**
     * Regression: long-pressing a showtime chip must reveal the room tooltip,
     * NOT the share menu. A card-wide long-press (the previous wiring) fired the
     * share menu on top of the chip's own long-press, hiding the room — this
     * pins the share gesture to the poster so the chip is left free.
     */
    @Test
    fun longPressOnShowtimeChipDoesNotOpenShareMenu() {
        var opened = false
        render(film = filmWithRoom, onOpen = { opened = true })

        // The chip's tag is a merged descendant of the card's clickable, so the
        // finder must read the unmerged tree.
        compose.onNodeWithTag(ShowtimeChipTestTag, useUnmergedTree = true)
            .performTouchInput { longClick() }
        compose.waitForIdle()

        compose.onNodeWithText("Udostępnij").assertDoesNotExist()
        compose.onNodeWithText("Skopiuj link").assertDoesNotExist()
        assertFalse("long-press on a chip must not open the detail screen", opened)
    }

    @Test
    fun tapOpensDetailAndShowsNoMenu() {
        var opened = false
        render(onOpen = { opened = true })

        compose.onNodeWithTag(FilmCardTestTag).performClick()
        compose.waitForIdle()

        compose.runOnIdle { assert(opened) { "tap should open the detail screen" } }
        compose.onNodeWithText("Udostępnij").assertDoesNotExist()
    }

    @Test
    fun copyMenuItemCopiesTheCanonicalLink() {
        render()

        compose.onNodeWithTag(FilmCardPosterTestTag).performTouchInput { longClick() }
        compose.waitForIdle()
        compose.onNodeWithText("Skopiuj link").performClick()
        compose.waitForIdle()

        val context = ApplicationProvider.getApplicationContext<Context>()
        val clipboard = context.getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager
        assertEquals(
            filmShareUrl("poznan", film.title),
            clipboard.primaryClip?.getItemAt(0)?.text?.toString(),
        )
    }
}
