package pl.kinowo.ui.detail

import android.view.View
import android.view.ViewGroup
import android.webkit.WebView
import androidx.activity.ComponentActivity
import androidx.compose.ui.test.junit4.createAndroidComposeRule
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.Shadows.shadowOf
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails

/**
 * The inline trailer must actually start. The player navigates the WebView
 * straight to the provider's embed page (a real https origin) — the previous
 * approach wrapped the embed in an `<iframe>` inside a `loadDataWithBaseURL`
 * document, whose synthetic origin keeps YouTube's player from ever starting,
 * so the embed rendered but stayed dead.
 *
 * Robolectric can't drive real video, so the reachable mechanism under test is
 * the load call: the WebView is pointed at the autoplay embed URL via
 * `loadUrl`, and the fragile `loadDataWithBaseURL` path is gone.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class TrailerPlayerTest {

    @get:Rule
    val compose = createAndroidComposeRule<ComponentActivity>()

    private val film = Film(title = "Diuna", posterURL = "https://example.com/diuna.jpg")

    @Test
    fun trailerEmbedUrlAppendsInlineAutoplay() {
        assertEquals(
            "https://www.youtube.com/embed/abcdefghijk?autoplay=1&playsinline=1",
            trailerEmbedUrl("https://www.youtube.com/embed/abcdefghijk"),
        )
        // A URL that already carries a query gets the params joined with '&'.
        assertEquals(
            "https://player.vimeo.com/video/12345?h=ab&autoplay=1&playsinline=1",
            trailerEmbedUrl("https://player.vimeo.com/video/12345?h=ab"),
        )
        // An explicit autoplay choice is left untouched.
        assertEquals(
            "https://www.youtube.com/embed/x?autoplay=0",
            trailerEmbedUrl("https://www.youtube.com/embed/x?autoplay=0"),
        )
    }

    @Test
    fun trailerNavigatesDirectlyToTheEmbedUrlSoItPlays() {
        val details = FilmDetails(
            title = "Diuna",
            trailerURLs = listOf("https://www.youtube.com/embed/abcdefghijk"),
        )
        compose.setContent { DetailScreen(film, details, onBack = {}) }
        compose.waitForIdle()

        val web = findWebView(compose.activity.window.decorView)
        requireNotNull(web) { "trailer WebView never composed" }
        val shadow = shadowOf(web)
        assertEquals(
            "https://www.youtube.com/embed/abcdefghijk?autoplay=1&playsinline=1",
            shadow.lastLoadedUrl,
        )
        // The old synthetic-origin iframe document is gone — the player needs a
        // genuine https navigation, not a loadDataWithBaseURL page.
        assertNull(
            "trailer must not fall back to the loadDataWithBaseURL iframe document",
            shadow.lastLoadDataWithBaseURL,
        )
    }

    private fun findWebView(view: View): WebView? = when (view) {
        is WebView -> view
        is ViewGroup -> (0 until view.childCount).firstNotNullOfOrNull { findWebView(view.getChildAt(it)) }
        else -> null
    }
}
