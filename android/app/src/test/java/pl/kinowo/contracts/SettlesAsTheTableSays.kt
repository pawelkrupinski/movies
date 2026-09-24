package pl.kinowo.contracts

import okhttp3.mockwebserver.MockWebServer
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail

/** Every `http:NNN` row of [source] in the retry-classification table, answered by
 *  [server] to each of [calls]: [isPermanent] agrees with the row's verdict, no call
 *  reads the status as success, and a call fails with the permanent-refusal error —
 *  [refusedStatus] non-null, naming the status — exactly when the row says permanent.
 *  The shape every client spec holding a classifier to the table shares. */
suspend fun MockWebServer.assertEveryCallSettlesAsTheTableSays(
    source: String,
    isPermanent: (Int) -> Boolean,
    refusedStatus: (Exception) -> Int?,
    calls: List<Pair<String, suspend () -> Any?>>,
) {
    val table = RetryClassificationTable.load()
    assertTrue("$source must list android as a consumer", source in table.sourcesConsumedBy("android"))
    val rows = table.rowsFor(source)
    assertTrue("$source has no rows", rows.isNotEmpty())
    for (row in rows) {
        val status = requireNotNull(row.status) { "$row names no status" }
        assertEquals("$row", row.isPermanent, isPermanent(status))
        answerEveryRequestWith(status)
        for ((name, call) in calls) {
            try {
                call()
                fail("a $status must not read as a successful $name")
            } catch (failure: Exception) {
                val refused = refusedStatus(failure)
                assertEquals("$name: $row", row.isPermanent, refused != null)
                if (row.isPermanent) assertEquals("$name: $row", status, refused)
            }
        }
    }
}
