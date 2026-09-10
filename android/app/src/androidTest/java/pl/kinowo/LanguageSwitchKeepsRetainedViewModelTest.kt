package pl.kinowo

import android.Manifest
import android.content.Context
import androidx.lifecycle.ViewModelProvider
import androidx.test.core.app.ActivityScenario
import androidx.test.core.app.ApplicationProvider
import androidx.test.ext.junit.runners.AndroidJUnit4
import androidx.test.platform.app.InstrumentationRegistry
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Assert.assertSame
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import pl.kinowo.data.UserPreferences
import pl.kinowo.ui.KinowoViewModel

/**
 * On-device mirror of [CountrySwitchRewiresApiTest], for the opposite claim:
 * a LANGUAGE switch must recreate the activity (so [pl.kinowo.ui.LocaleWrapper]
 * re-applies with the new tag — asserted here via a resource string that reads
 * differently per language) but must NOT drop the retained [KinowoViewModel] —
 * unlike a country switch, a pure language change never touches [pl.kinowo.net.KinowoApi]'s
 * base URL, so clearing the ViewModelStore would just be a wasted reload. See
 * [MainActivity.onCreate]'s two independent watchers.
 *
 * Run with `./gradlew app:connectedDebugAndroidTest --tests
 * pl.kinowo.LanguageSwitchKeepsRetainedViewModelTest` (or via `android/scripts/devtest.sh`).
 */
@RunWith(AndroidJUnit4::class)
class LanguageSwitchKeepsRetainedViewModelTest {

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
        lateinit var vm: KinowoViewModel
        scenario.onActivity { vm = ViewModelProvider(it)[KinowoViewModel::class.java] }
        return vm
    }

    private fun currentLanguageLabel(scenario: ActivityScenario<MainActivity>): String {
        lateinit var label: String
        scenario.onActivity { label = it.getString(R.string.filter_language) }
        return label
    }

    @Test
    fun switchingLanguageRecreatesButKeepsTheRetainedViewModel() {
        val context = ApplicationProvider.getApplicationContext<Context>()
        runBlocking { UserPreferences(context).setLanguageTag("pl") }

        ActivityScenario.launch(MainActivity::class.java).use { scenario ->
            val before = currentViewModel(scenario)
            assertEquals("Język", currentLanguageLabel(scenario))

            // Switch to German. MainActivity observes the persisted tag and
            // recreate()s — WITHOUT clearing the ViewModelStore.
            runBlocking { UserPreferences(context).setLanguageTag("de") }

            // Wait for the locale to actually flip, polling rather than a fixed
            // sleep so the test isn't racy.
            var label = currentLanguageLabel(scenario)
            var tries = 0
            while (label != "Sprache" && tries < 50) {
                Thread.sleep(200)
                label = currentLanguageLabel(scenario)
                tries++
            }
            assertEquals("the new locale must be forced after the recreate", "Sprache", label)

            assertSame(
                "A pure language switch must NOT drop the retained ViewModel — it " +
                    "never touches KinowoApi's base URL, so clearing it would just force " +
                    "a wasteful full data reload for a change that only needed new strings.",
                before,
                currentViewModel(scenario),
            )
        }
    }
}
