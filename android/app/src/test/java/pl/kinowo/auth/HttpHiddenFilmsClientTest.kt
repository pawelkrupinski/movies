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
        val table = RetryClassificationTable.load()
        assertTrue("user-state:hidden-films-write" in table.sourcesConsumedBy("android"))
        val rows = table.rowsFor("user-state:hidden-films-write")
        assertTrue(rows.isNotEmpty())
        val writes: List<Pair<String, suspend () -> HiddenFilmsState>> = listOf(
            "hide" to { client.hide("pl", "Film") },
            "unhide" to { client.unhide("pl", "Film") },
            "clear" to { client.clear("pl") },
        )
        for (row in rows) {
            val status = requireNotNull(row.status) { "$row names no status" }
            assertEquals("$row", row.isPermanent, HiddenFilmsWriteRefused.isPermanent(status))
            server.answerEveryRequestWith(status)
            for ((name, write) in writes) {
                try {
                    write()
                    fail("a $status must not read as a successful $name")
                } catch (failure: Exception) {
                    assertEquals("$name: $row", row.isPermanent, failure is HiddenFilmsWriteRefused)
                    if (row.isPermanent) assertEquals(status, (failure as HiddenFilmsWriteRefused).statusCode)
                }
            }
        }
    }
}
