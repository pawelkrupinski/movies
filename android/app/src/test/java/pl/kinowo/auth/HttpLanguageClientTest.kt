package pl.kinowo.auth

import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import okhttp3.mockwebserver.MockWebServer
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Before
import org.junit.Test
import pl.kinowo.contracts.RetryClassificationTable
import pl.kinowo.contracts.answerEveryRequestWith

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
        val table = RetryClassificationTable.load()
        assertTrue("user-state:language-push" in table.sourcesConsumedBy("android"))
        val rows = table.rowsFor("user-state:language-push")
        assertTrue(rows.isNotEmpty())
        for (row in rows) {
            val status = requireNotNull(row.status) { "$row names no status" }
            assertEquals("$row", row.isPermanent, LanguagePushRefused.isPermanent(status))
            server.answerEveryRequestWith(status)
            try {
                client.push("de")
                fail("a $status must not read as a successful push")
            } catch (failure: Exception) {
                assertEquals("$row", row.isPermanent, failure is LanguagePushRefused)
                if (row.isPermanent) assertEquals(status, (failure as LanguagePushRefused).statusCode)
            }
        }
    }
}
