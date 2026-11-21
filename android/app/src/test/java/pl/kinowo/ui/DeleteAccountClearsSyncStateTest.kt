package pl.kinowo.ui

import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.KinowoViewModelHarness
import pl.kinowo.contracts.MockWebServerRule
import pl.kinowo.contracts.answerEveryRequestWith
import pl.kinowo.data.UserPreferences

/**
 * [KinowoViewModel.deleteAccount] wipes local hiddenFilms/disabledCinemas, but
 * used to leave every country's `hiddenFilmsMigrated`/validators flags behind.
 * A stale "migrated" flag surviving into the NEXT login (same device, fresh
 * account) would make `StateSyncService.reconcile` treat that country as
 * already synced, skip the union, and mirror back whatever the fresh account's
 * empty server row has — silently dropping anything set locally before
 * signing back in.
 *
 * This test was deleted once for hanging the JVM when it shared a Gradle fork
 * with `CitySwitchSuppressionWiringTest` — see [pl.kinowo.KinowoViewModelHarness]
 * for the wedged-DataStore-lock root cause and the teardown that fixes it.
 *
 * The wipe happens only once the server answered a 2xx: it answers 503 when
 * its user store is unreachable and the account was not deleted, and then the
 * app keeps every local pref and says so.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class DeleteAccountClearsSyncStateTest {

    @get:Rule
    val harness = KinowoViewModelHarness()

    // Every request, the ViewModel's own session check included, gets one status.
    @get:Rule
    val mock = MockWebServerRule()

    private fun syncedPrefs() = UserPreferences(harness.context).also { prefs ->
        runBlocking {
            prefs.setHiddenFilmsMigrated("pl", true)
            prefs.setHiddenFilmsValidators("pl", "\"etag\"", "Tue, 19 May 2026 12:00:00 GMT")
        }
    }

    @Test
    fun deleteAccountClearsThePerCountryHiddenFilmsMigrationFlag() {
        mock.server.answerEveryRequestWith(204)
        val prefs = syncedPrefs()
        val vm = harness.viewModel(prefs = prefs, authBaseUrl = mock.baseUrl)

        harness.settle(vm.deleteAccount())

        runBlocking {
            assertFalse(
                "deleteAccount() must clear the per-country migration flag, or the next " +
                    "login (a different account on this device) would wrongly treat 'pl' as already synced",
                prefs.isHiddenFilmsMigrated("pl"),
            )
            assertNull(prefs.hiddenFilmsEtag("pl"))
        }
    }

    @Test
    fun aDeletionTheServerRefusedKeepsTheLocalPrefsAndSaysSo() {
        mock.server.answerEveryRequestWith(503)
        val prefs = syncedPrefs()
        val vm = harness.viewModel(prefs = prefs, authBaseUrl = mock.baseUrl)

        harness.settle(vm.deleteAccount())

        assertTrue("a 503 must surface the failure", vm.deleteAccountFailed.value)
        runBlocking {
            assertTrue(
                "the server kept the account, so its local sync state must stay too",
                prefs.isHiddenFilmsMigrated("pl"),
            )
            assertEquals("\"etag\"", prefs.hiddenFilmsEtag("pl"))
        }
        vm.dismissDeleteAccountFailure()
        assertFalse(vm.deleteAccountFailed.value)
    }
}
