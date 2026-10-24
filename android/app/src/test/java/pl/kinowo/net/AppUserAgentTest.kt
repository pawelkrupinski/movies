package pl.kinowo.net

import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import okhttp3.mockwebserver.MockResponse
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import pl.kinowo.contracts.MockWebServerRule

/**
 * The app's User-Agent names its build and the Android release it runs on, behind the
 * `KinowoAndroid/` prefix the server's client classification matches.
 */
class AppUserAgentTest {

    @get:Rule
    val mock = MockWebServerRule()

    @Test
    fun namesTheVersionNameTheReleaseAndTheApiLevel() {
        assertEquals("KinowoAndroid/2.1.3 (Android 15; API 35)", appUserAgent("2.1.3", "15", 35))
    }

    @Test
    fun anUnreadableVersionNameIsZero() {
        assertEquals("KinowoAndroid/0 (Android 8.0.0; API 26)", appUserAgent("", "8.0.0", 26))
    }

    @Test
    fun aMissingReleaseIsUnknown() {
        assertEquals("KinowoAndroid/2.1 (Android unknown; API 0)", appUserAgent("2.1", null, 0))
        assertEquals("KinowoAndroid/2.1 (Android unknown; API 0)", appUserAgent("2.1", "", 0))
    }

    @Test
    fun theApiSendsTheAgentItWasGiven() = runBlocking {
        mock.server.enqueue(MockResponse().setBody("[]"))
        val agent = appUserAgent("2.1.3", "15", 35)
        KinowoApi(baseUrl = mock.baseUrl, client = OkHttpClient(), userAgent = agent)
            .fetchRepertoire(citySlug = "poznan", ifModifiedSince = null)
        assertEquals(agent, mock.server.takeRequest().getHeader("User-Agent"))
    }
}
