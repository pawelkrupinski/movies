package pl.kinowo

import android.Manifest
import android.content.Context
import androidx.lifecycle.ViewModelProvider
import androidx.test.core.app.ActivityScenario
import androidx.test.core.app.ApplicationProvider
import androidx.test.ext.junit.runners.AndroidJUnit4
import androidx.test.platform.app.InstrumentationRegistry
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertNotSame
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import pl.kinowo.data.UserPreferences
import pl.kinowo.ui.KinowoViewModel

/**
 * On-device regression for the in-session country switch. Each country is its
 * own web deployment ([pl.kinowo.model.Country.baseUrl]); [MainActivity] bakes
 * that base URL into the [pl.kinowo.net.KinowoApi] it wires in the
 * `by viewModels { … }` factory. Switching country persists the new code and
 * calls `recreate()`.
 *
 * The bug: `recreate()` RETAINS the ViewModel (exactly like a configuration
 * change), so the factory never re-ran and the retained KinowoApi kept the OLD
 * country's base URL — the UI flipped to the new locale while repertoire
 * requests still hit the previous deployment (a UK city fetched from the PL
 * backend returns an empty 200, rendering as "no showings"). The fix clears the
 * ViewModelStore before `recreate()` so the graph re-wires at the new base URL.
 *
 * We can't read the private base URL, but we can observe the mechanism: after a
 * country change the activity must hold a DIFFERENT [KinowoViewModel] instance
 * (the old one, with the stale KinowoApi, was dropped). Before the fix the
 * retained instance is reused (same reference) and this fails; after it, a fresh
 * instance is built and it passes.
 *
 * Run with `./gradlew app:connectedDebugAndroidTest --tests
 * pl.kinowo.CountrySwitchRewiresApiTest` (or via `android/scripts/devtest.sh`).
 */
@RunWith(AndroidJUnit4::class)
class CountrySwitchRewiresApiTest {

    // MainActivity requests approximate location on launch; pre-grant it so the
    // system dialog never steals focus (which would leave the activity paused and
    // ActivityScenario waiting for RESUMED).
    @Before
    fun grantLocation() {
        val pkg = ApplicationProvider.getApplicationContext<Context>().packageName
        InstrumentationRegistry.getInstrumentation().uiAutomation
            .grantRuntimePermission(pkg, Manifest.permission.ACCESS_COARSE_LOCATION)
    }

    private fun currentViewModel(scenario: ActivityScenario<MainActivity>): KinowoViewModel {
        // The activity already created the VM in its own store (via `by viewModels`),
        // so ViewModelProvider returns that existing instance without invoking a
        // factory — we just read whichever instance the CURRENT activity holds.
        lateinit var vm: KinowoViewModel
        scenario.onActivity { vm = ViewModelProvider(it)[KinowoViewModel::class.java] }
        return vm
    }

    @Test
    fun switchingCountryDropsTheRetainedViewModel() {
        val context = ApplicationProvider.getApplicationContext<Context>()
        runBlocking { UserPreferences(context).setCountryCode("pl") }

        ActivityScenario.launch(MainActivity::class.java).use { scenario ->
            val before = currentViewModel(scenario)

            // Switch to the UK deployment. MainActivity observes the persisted code
            // and, on a real change, clears the store + recreate()s.
            runBlocking { UserPreferences(context).setCountryCode("uk") }

            // Wait for the self-triggered recreate to settle, then read the current
            // activity's VM. Poll rather than fixed-sleep so the test isn't racy.
            var after = before
            var tries = 0
            while (after === before && tries < 50) {
                Thread.sleep(200)
                after = currentViewModel(scenario)
                tries++
            }

            assertNotSame(
                "Switching country must rebuild the ViewModel so KinowoApi re-points " +
                    "to the new country's baseUrl (retaining it leaves UK requests hitting " +
                    "the PL backend, which returns empty).",
                before,
                after,
            )
        }
    }
}
