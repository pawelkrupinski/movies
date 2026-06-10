package pl.kinowo.ui.detail

import android.view.View
import android.view.ViewGroup
import android.webkit.WebView
import androidx.activity.ComponentActivity
import androidx.compose.ui.test.junit4.createAndroidComposeRule
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
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
 * The inline trailer must actually start. YouTube embed URLs are built for
 * iframes: loading one as a top-level `loadUrl` navigation renders the page but
 * the player refuses to start (no embedding referer), and wrapping it under a
 * fake `youtube.com` base origin breaks the player's parent handshake. The
 * working shape — proven on iOS (TrailerEmbedHTML under the `kinowo.fly.dev`
 * base) — is an iframe document loaded via `loadDataWithBaseURL` under our real
 * site origin, the same referer YouTube already permits playback for on the web.
 *
 * Robolectric can't drive real video, so the reachable mechanism under test is
 * the load call: the WebView gets the iframe HTML (carrying the autoplay embed
 * src) via `loadDataWithBaseURL` under `kinowo.fly.dev`, and never the dead
 * top-level `loadUrl` navigation.
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
    fun trailerLoadsTheIframeUnderTheSiteOriginSoItPlays() {
        val details = FilmDetails(
            title = "Diuna",
            trailerURLs = listOf("https://www.youtube.com/embed/abcdefghijk"),
        )
        compose.setContent { DetailScreen(film, details, onBack = {}) }
        compose.waitForIdle()

        val web = findWebView(compose.activity.window.decorView)
        requireNotNull(web) { "trailer WebView never composed" }
        val shadow = shadowOf(web)

        // A top-level navigation to the embed URL is the dead path — YouTube
        // refuses to play it. The player must use the iframe document instead.
        assertNull(
            "trailer must not navigate top-level to the embed URL",
            shadow.lastLoadedUrl,
        )

        val loaded = shadow.lastLoadDataWithBaseURL
        requireNotNull(loaded) { "trailer must load an iframe document, not a top-level URL" }
        // The iframe must be embedded under our real site origin — not a fake
        // youtube.com base (breaks the player handshake) — so YouTube sees the
        // same allowed referer it does on the web.
        assertEquals("https://kinowo.fly.dev", loaded.baseUrl)
        assertTrue("expected an <iframe> document, got: ${loaded.data}", loaded.data.contains("<iframe"))
        assertTrue(
            "iframe src should carry the autoplay embed URL, got: ${loaded.data}",
            loaded.data.contains("https://www.youtube.com/embed/abcdefghijk?autoplay=1&amp;playsinline=1"),
        )
    }

    private fun findWebView(view: View): WebView? = when (view) {
        is WebView -> view
        is ViewGroup -> (0 until view.childCount).firstNotNullOfOrNull { findWebView(view.getChildAt(it)) }
        else -> null
    }
}
