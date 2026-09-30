package pl.kinowo.auth

import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import org.junit.Rule
import org.junit.Test
import pl.kinowo.contracts.MockWebServerRule
import pl.kinowo.contracts.assertEveryCallSettlesAsTheTableSays

/** [HttpLanguageClient.push] tells a pick the server refuses for good (a
 *  language it does not know) from a failure worth retrying, so
 *  [StateSyncService] can stop owing the one and keep the other. Mirrors iOS
 *  `HttpLanguageClientTests`. */
class HttpLanguageClientTest {

    @get:Rule
    val mock = MockWebServerRule()
    private val server get() = mock.server
    private val client by lazy { HttpLanguageClient(baseUrl = mock.baseUrl, client = OkHttpClient()) }

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
