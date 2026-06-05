package pl.kinowo.net

import kotlinx.coroutines.runBlocking
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
        api = KinowoApi(baseUrl = server.url("").toString().trimEnd('/'))
    }

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
}
