package pl.kinowo.net

import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import okhttp3.mockwebserver.MockResponse
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Rule
import org.junit.Test
import pl.kinowo.contracts.MockWebServerRule
import java.io.IOException

/**
 * Pins the city-slug prefix [KinowoApi] builds into the request path. The
 * server mounts every API under `/{city}/…`, so a missing or wrong prefix would
 * hit the bare (now-404) path; this records the actual request against a
 * MockWebServer so the prefix can't silently regress.
 */
class KinowoApiPathTest {

    @get:Rule
    val mock = MockWebServerRule()
    private val server get() = mock.server
    private val api by lazy { KinowoApi(baseUrl = mock.baseUrl, client = OkHttpClient()) }

    @Test
    fun repertoirePathCarriesTheCitySlug() = runBlocking {
        server.enqueue(MockResponse().setBody("[]"))
        api.fetchRepertoire(citySlug = "poznan", ifModifiedSince = null)
        assertEquals("/poznan/api/repertoire", server.takeRequest().path)
    }

    @Test
    fun detailsPathCarriesTheCitySlug() = runBlocking {
        server.enqueue(MockResponse().setBody("[]"))
        api.fetchDetails(citySlug = "poznan", ifModifiedSince = null)
        assertEquals("/poznan/api/details", server.takeRequest().path)
    }

    @Test
    fun clientSupportIsCountryAgnostic() = runBlocking {
        server.enqueue(MockResponse().setBody("{}"))
        assertEquals("{}", api.fetchClientSupport())
        assertEquals("/api/client-support", server.takeRequest().path)
    }

    /** A non-2xx is a failure (which the gate then treats as "supported"), never
     *  an error page handed on as if it were the payload. */
    @Test
    fun clientSupportNon2xxThrows() {
        server.enqueue(MockResponse().setResponseCode(503).setBody("down"))
        assertThrows(IOException::class.java) { runBlocking { api.fetchClientSupport() } }
    }

    /** The client is the composition root's, not one the API builds for
     *  itself: every request goes through the instance it was handed (in the
     *  app, the one carrying the session cookie jar). */
    @Test
    fun requestsGoThroughTheClientItIsGiven() = runBlocking {
        val given = OkHttpClient.Builder()
            .addInterceptor { chain ->
                chain.proceed(chain.request().newBuilder().header("X-Given-Client", "yes").build())
            }
            .build()
        server.enqueue(MockResponse().setBody("[]"))
        KinowoApi(baseUrl = mock.baseUrl, client = given).fetchRepertoire(citySlug = "poznan", ifModifiedSince = null)
        assertEquals("yes", server.takeRequest().getHeader("X-Given-Client"))
    }
}
