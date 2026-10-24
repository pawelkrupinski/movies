package pl.kinowo

import android.os.Looper
import androidx.test.core.app.ApplicationProvider
import coil3.request.ImageRequest
import kotlinx.coroutines.runBlocking
import okhttp3.mockwebserver.MockResponse
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.Shadows.shadowOf
import org.robolectric.annotation.Config
import pl.kinowo.contracts.MockWebServerRule
import pl.kinowo.net.KinowoApi
import java.util.concurrent.TimeUnit
import pl.kinowo.net.TEST_USER_AGENT

/**
 * Posters go through the app's one OkHttp client, not a second client Coil
 * would otherwise build for itself with its own connection pool and dispatcher:
 * a poster fetched after an API call to the same host rides the API call's
 * kept-alive connection. Poster requests still carry no cookies — cinema CDNs'
 * Set-Cookie must not land in the persisted session jar, as it never did while
 * Coil had its own cookie-less client.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class SharedHttpClientTest {

    @get:Rule
    val mock = MockWebServerRule()
    private val server get() = mock.server
    private val app get() = ApplicationProvider.getApplicationContext<KinowoApplication>()

    @Test
    fun aPosterReusesTheApiCallsConnection() = runBlocking {
        server.enqueue(MockResponse().setBody("[]"))
        server.enqueue(MockResponse().setBody("poster bytes"))

        KinowoApi(baseUrl = mock.baseUrl, client = app.httpClient, userAgent = TEST_USER_AGENT).fetchRepertoire("poznan", ifModifiedSince = null)
        loadPoster()

        assertEquals(0, takeRequest().sequenceNumber)
        assertEquals("the poster's request is the second on the API call's connection", 1, takeRequest().sequenceNumber)
    }

    @Test
    fun aPosterRequestCarriesNoSessionCookie() = runBlocking {
        server.enqueue(MockResponse().setBody("[]").addHeader("Set-Cookie", "PLAY_SESSION=signed-in; Path=/"))
        server.enqueue(MockResponse().setBody("poster bytes"))

        KinowoApi(baseUrl = mock.baseUrl, client = app.httpClient, userAgent = TEST_USER_AGENT).fetchRepertoire("poznan", ifModifiedSince = null)
        loadPoster()

        takeRequest()
        assertNull(takeRequest().getHeader("Cookie"))
    }

    // Enqueued, not `execute`d: Coil's pipeline starts on the main dispatcher,
    // which a runBlocking test body would be parking. The main looper is
    // idled until the load (any outcome) settles.
    private fun loadPoster() {
        val load = app.newImageLoader(app).enqueue(ImageRequest.Builder(app).data("${mock.baseUrl}/poster.jpg").build())
        val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10)
        while (!load.job.isCompleted && System.nanoTime() < deadline) {
            shadowOf(Looper.getMainLooper()).idle()
            Thread.sleep(10)
        }
        assertTrue("the poster load settles", load.job.isCompleted)
    }

    private fun takeRequest() = server.takeRequest(5, TimeUnit.SECONDS) ?: error("no request reached the server")
}
