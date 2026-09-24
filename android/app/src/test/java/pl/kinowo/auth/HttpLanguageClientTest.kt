package pl.kinowo.auth

import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import okhttp3.mockwebserver.MockResponse
import okhttp3.mockwebserver.MockWebServer
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.fail
import org.junit.Before
import org.junit.Test

/** [HttpLanguageClient.push] tells a pick the server refuses for good (a
 *  language it does not know) from a failure worth retrying, so
 *  [StateSyncService] can stop owing the one and keep the other. Mirrors iOS
 *  `HttpLanguageClientTests`. */
class HttpLanguageClientTest {

    private lateinit var server: MockWebServer
    private lateinit var client: HttpLanguageClient

    @Before
    fun setUp() {
        server = MockWebServer()
        server.start()
        client = HttpLanguageClient(baseUrl = server.url("").toString().trimEnd('/'), client = OkHttpClient())
    }

    @After
    fun tearDown() {
        server.shutdown()
    }

    @Test
    fun aFourHundredIsAPermanentRefusal() = runBlocking {
        server.enqueue(MockResponse().setResponseCode(400))
        try {
            client.push("it")
            fail("a 400 must not read as a successful push")
        } catch (refused: LanguagePushRefused) {
            assertEquals(400, refused.statusCode)
        }
    }

    @Test
    fun everythingButAFourHundredIsWorthRetrying() = runBlocking {
        // 403 included: a Cloudflare challenge in front of the app is a 403 too.
        for (status in listOf(401, 403, 404, 408, 422, 429, 503)) {
            server.enqueue(MockResponse().setResponseCode(status))
            try {
                client.push("de")
                fail("a $status must not read as a successful push")
            } catch (failure: Exception) {
                assertFalse("$status is worth retrying", failure is LanguagePushRefused)
            }
        }
    }
}
