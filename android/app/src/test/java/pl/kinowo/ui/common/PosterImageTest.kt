package pl.kinowo.ui.common

import android.graphics.Bitmap
import android.os.Looper
import androidx.compose.foundation.layout.size
import androidx.compose.ui.Modifier
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onAllNodesWithTag
import androidx.compose.ui.test.onNodeWithContentDescription
import androidx.compose.ui.test.onNodeWithTag
import androidx.compose.ui.unit.dp
import androidx.test.core.app.ApplicationProvider
import coil3.ImageLoader
import coil3.SingletonImageLoader
import coil3.annotation.DelicateCoilApi
import okhttp3.mockwebserver.MockResponse
import okio.Buffer
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.Shadows.shadowOf
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.contracts.MockWebServerRule
import java.io.ByteArrayOutputStream
import java.util.concurrent.TimeUnit

/**
 * [PosterImage]'s three faces: the film-strip glyph while a poster loads and
 * once its whole fallback chain has failed, the poster itself on success —
 * and the poster's description stays on the node throughout, so a screen
 * reader names the card before its bytes arrive.
 */
@OptIn(DelicateCoilApi::class) // SingletonImageLoader.setUnsafe / reset
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class PosterImageTest {

    @get:Rule
    val compose = createComposeRule()

    @get:Rule
    val mock = MockWebServerRule()

    @Before
    fun cacheFreeLoader() {
        val context = ApplicationProvider.getApplicationContext<android.content.Context>()
        SingletonImageLoader.setUnsafe(ImageLoader.Builder(context).memoryCache(null).diskCache(null).build())
    }

    @After
    fun resetLoader() = SingletonImageLoader.reset()

    private fun show(vararg paths: String) = compose.setContent {
        PosterImage(chain = paths.map { "${mock.baseUrl}$it" }, contentDescription = "Poster", modifier = Modifier.size(90.dp, 135.dp))
    }

    @Test
    fun whileLoadingTheGlyphShowsAndThePosterKeepsItsDescription() {
        mock.server.enqueue(MockResponse().setHeadersDelay(10, TimeUnit.SECONDS))
        show("/slow.jpg")

        awaitRequests(1)
        compose.onNodeWithTag(PosterPlaceholderTag).assertExists()
        compose.onNodeWithContentDescription("Poster").assertExists()
    }

    @Test
    fun aFailingPosterFallsThroughTheChainThenShowsTheGlyph() {
        mock.server.enqueue(MockResponse().setResponseCode(404))
        mock.server.enqueue(MockResponse().setResponseCode(403))
        show("/first.jpg", "/second.jpg")

        awaitRequests(2)
        compose.waitForIdle()
        assertEquals("/first.jpg", mock.server.takeRequest().path)
        assertEquals("/second.jpg", mock.server.takeRequest().path)
        compose.onNodeWithTag(PosterPlaceholderTag).assertExists()
    }

    @Test
    fun aLoadedPosterReplacesTheGlyph() {
        mock.server.enqueue(MockResponse().setResponseCode(404))
        mock.server.enqueue(MockResponse().setBody(Buffer().write(png())))
        show("/missing.jpg", "/poster.png")

        compose.waitUntil(5_000) { compose.onAllNodesWithTag(PosterPlaceholderTag).fetchSemanticsNodes().isEmpty() }
        compose.onNodeWithContentDescription("Poster").assertExists()
    }

    // Coil posts its results to the main looper from its own threads; a
    // condition that reads no UI state has to run that looper itself.
    private fun awaitRequests(count: Int) = compose.waitUntil(5_000) {
        shadowOf(Looper.getMainLooper()).idle()
        mock.server.requestCount >= count
    }

    private fun png(): ByteArray {
        val bitmap = Bitmap.createBitmap(20, 30, Bitmap.Config.ARGB_8888).apply { eraseColor(android.graphics.Color.RED) }
        return ByteArrayOutputStream().also { bitmap.compress(Bitmap.CompressFormat.PNG, 100, it) }.toByteArray()
    }
}

