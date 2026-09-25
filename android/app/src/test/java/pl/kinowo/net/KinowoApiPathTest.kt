package pl.kinowo.net

import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import okhttp3.mockwebserver.MockResponse
import okhttp3.mockwebserver.MockWebServer
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.Test

/**
 * Pins the city-slug prefix [KinowoApi] builds into the request path. The
 * server mounts every API under `/{city}/…`, so a missing or wrong prefix would
 * hit the bare (now-404) path; this records the actual request against a
 * MockWebServer so the prefix can't silently regress.
 */
class KinowoApiPathTest {

    private lateinit var server: MockWebServer
    private lateinit var api: KinowoApi

    @Before
    fun setUp() {
        server = MockWebServer()
        server.start()
        api = KinowoApi(baseUrl = baseUrl(), client = OkHttpClient())
    }

    private fun baseUrl() = server.url("").toString().trimEnd('/')

    @After
    fun tearDown() {
        server.shutdown()
    }

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
        KinowoApi(baseUrl = baseUrl(), client = given).fetchRepertoire(citySlug = "poznan", ifModifiedSince = null)
        assertEquals("yes", server.takeRequest().getHeader("X-Given-Client"))
    }
}
