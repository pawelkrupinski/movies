package pl.kinowo.ui

import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.KinowoViewModelHarness
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
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class DeleteAccountClearsSyncStateTest {

    @get:Rule
    val harness = KinowoViewModelHarness()

    @Test
    fun deleteAccountClearsThePerCountryHiddenFilmsMigrationFlag() {
        val prefs = UserPreferences(harness.context)
        runBlocking {
            prefs.setHiddenFilmsMigrated("pl", true)
            prefs.setHiddenFilmsValidators("pl", "\"etag\"", "Tue, 19 May 2026 12:00:00 GMT")
        }
        val vm = harness.viewModel(prefs = prefs)

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
}
