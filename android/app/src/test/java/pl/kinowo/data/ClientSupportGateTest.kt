package pl.kinowo.data

import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test
import pl.kinowo.net.ClientSupportApi
import java.io.IOException

/** The launch decision: the update screen's store URL, or null to carry on. */
class ClientSupportGateTest {

    private fun body(androidMinimum: String) =
        """{"android":{"minimumVersion":"$androidMinimum","storeUrl":"$STORE"},"ios":{"minimumVersion":"1.0","storeUrl":"https://apps.apple.com/app/id1"}}"""

    private fun decide(appVersion: String, api: ClientSupportApi): String? = runBlocking {
        ClientSupportGate(api, appVersion).run { check(); updateStoreUrl.value }
    }

    @Test
    fun aVersionBelowTheMinimumIsSentToTheStore() {
        assertEquals(STORE, decide("1.0", { body("1.1") }))
    }

    @Test
    fun aVersionAtOrAboveTheMinimumCarriesOn() {
        assertNull(decide("1.1", { body("1.1") }))
        assertNull(decide("1.10", { body("1.9") }))
    }

    @Test
    fun onlyTheAndroidHalfDecides() {
        val iosGatedOnly = """{"android":{"minimumVersion":null,"storeUrl":null},"ios":{"minimumVersion":"9.0","storeUrl":"https://x"}}"""
        assertNull(decide("1.0", { iosGatedOnly }))
    }

    @Test
    fun aFailedFetchNeverLocksTheUserOut() {
        assertNull(decide("1.0", { throw IOException("HTTP 503") }))
    }

    @Test
    fun anUnparseableBodyNeverLocksTheUserOut() {
        assertNull(decide("1.0", { "<html>gateway timeout</html>" }))
    }

    @Test
    fun anUnreadableAppVersionNeverLocksTheUserOut() {
        assertNull(decide("", { body("99.0") }))
    }

    private companion object {
        const val STORE = "https://play.google.com/store/apps/details?id=net.pawel.kinowo"
    }
}
