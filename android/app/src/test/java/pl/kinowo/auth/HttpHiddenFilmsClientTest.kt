package pl.kinowo.auth

import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import okhttp3.mockwebserver.MockWebServer
import org.junit.After
import org.junit.Before
import org.junit.Test
import pl.kinowo.contracts.assertEveryCallSettlesAsTheTableSays

/** [HttpHiddenFilmsClient]'s writes tell an edit the server refuses for good
 *  from a failure worth resending, so [StateSyncService] can stop owing the
 *  one and keep the other queued. Mirrors iOS `HttpHiddenFilmsClientTests`. */
class HttpHiddenFilmsClientTest {

    private lateinit var server: MockWebServer
    private lateinit var client: HttpHiddenFilmsClient

    @Before
    fun setUp() {
        server = MockWebServer()
        server.start()
        client = HttpHiddenFilmsClient(baseUrl = server.url("").toString().trimEnd('/'), client = OkHttpClient())
    }

    @After
    fun tearDown() {
        server.shutdown()
    }

    /** Only 400 (over-long title, unknown country) and 413 (full bucket) are
     *  refused for good; a 403 in particular is as likely a Cloudflare challenge
     *  in front of the app. Every status is a row of the repo's
     *  retry-classification table, which the web and iOS hold their own rule to
     *  as well — for a hide, an unhide and a clear alike. */
    @Test
    fun everyWriteSettlesEveryStatusAsTheRetryClassificationTableSays() = runBlocking {
        server.assertEveryCallSettlesAsTheTableSays(
            source = "user-state:hidden-films-write",
            isPermanent = { HiddenFilmsWriteRefused.isPermanent(it) },
            refusedStatus = { (it as? HiddenFilmsWriteRefused)?.statusCode },
            calls = listOf(
                "hide" to { client.hide("pl", "Film") },
                "unhide" to { client.unhide("pl", "Film") },
                "clear" to { client.clear("pl") },
            ),
        )
    }
}
