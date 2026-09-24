package pl.kinowo.auth

import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import okhttp3.mockwebserver.MockWebServer
import org.junit.After
import org.junit.Before
import org.junit.Test
import pl.kinowo.contracts.assertEveryCallSettlesAsTheTableSays

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

    /** Only a 400 (a language this server does not know) is refused for good; a
     *  403 in particular is as likely a Cloudflare challenge in front of the app.
     *  Every status is a row of the repo's retry-classification table, which the
     *  web and iOS hold their own rule to as well. */
    @Test
    fun everyStatusSettlesAsTheRetryClassificationTableSays() = runBlocking {
        server.assertEveryCallSettlesAsTheTableSays(
            source = "user-state:language-push",
            isPermanent = { LanguagePushRefused.isPermanent(it) },
            refusedStatus = { (it as? LanguagePushRefused)?.statusCode },
            calls = listOf("push" to { client.push("de") }),
        )
    }
}
