package pl.kinowo.ui.list

import android.content.ClipboardManager
import android.content.Context
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
import pl.kinowo.ui.common.filmShareUrl

/**
 * Long-pressing a [FilmCard] opens a share menu — Udostępnij (system share
 * sheet) and Skopiuj link (clipboard) — mirroring iOS `FilmCardView`'s
 * `.contextMenu`. Before this menu existed the long-press copied the link
 * outright, so the "Udostępnij" item asserted here did not appear; a plain tap
 * still opens the detail screen and shows no menu.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class FilmCardShareMenuTest {

    @get:Rule
    val compose = createComposeRule()

    private val film = TestData.film(title = "Diuna: Część druga", days = emptyList())

    private fun render(onOpen: () -> Unit = {}) {
        compose.setContent {
            FilmCard(film = film, showCinemaHeaders = true, onOpen = onOpen, onHide = {})
        }
    }

    @Test
    fun longPressOpensShareMenu() {
        var opened = false
        render(onOpen = { opened = true })

        compose.onNodeWithTag(FilmCardTestTag).performTouchInput { longClick() }
        compose.waitForIdle()

        compose.onNodeWithText("Udostępnij").assertExists()
        compose.onNodeWithText("Skopiuj link").assertExists()
        assertFalse("long-press must not open the detail screen", opened)
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

        compose.onNodeWithTag(FilmCardTestTag).performTouchInput { longClick() }
        compose.waitForIdle()
        compose.onNodeWithText("Skopiuj link").performClick()
        compose.waitForIdle()

        val context = ApplicationProvider.getApplicationContext<Context>()
        val clipboard = context.getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager
        assertEquals(
            filmShareUrl(film.title),
            clipboard.primaryClip?.getItemAt(0)?.text?.toString(),
        )
    }
}
