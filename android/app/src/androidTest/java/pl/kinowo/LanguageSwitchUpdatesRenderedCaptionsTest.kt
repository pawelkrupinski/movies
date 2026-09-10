package pl.kinowo

import android.Manifest
import android.content.Context
import androidx.compose.ui.test.junit4.createEmptyComposeRule
import androidx.compose.ui.test.onAllNodesWithText
import androidx.compose.ui.test.onNodeWithText
import androidx.test.core.app.ActivityScenario
import androidx.test.core.app.ApplicationProvider
import androidx.test.ext.junit.runners.AndroidJUnit4
import androidx.test.platform.app.InstrumentationRegistry
import kotlinx.coroutines.runBlocking
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import pl.kinowo.data.UserPreferences

/**
 * On-device companion to [LanguageSwitchKeepsRetainedViewModelTest]. That test
 * proves the FORCED LOCALE resolves a new resource string via a raw
 * `Activity.getString` call — it does not prove the on-screen Compose UI
 * actually repaints with the new strings, since `getString` bypasses
 * composition entirely. This test drives the same real switch (persist a new
 * tag → MainActivity's `recreate()` watcher → [pl.kinowo.ui.LocaleWrapper])
 * and asserts a LIVE, rendered caption — the "Dziś"/"Today" date pill in the
 * top bar (see [pl.kinowo.ui.list.DateBar]) — actually updates, closing the
 * gap a resource-only assertion leaves open.
 *
 * A prior investigation reported this pill (plus the "All cinemas" row and the
 * sort-dropdown labels) staying in the old language after a switch. That was
 * not reproducible against a real build (see the investigation notes on this
 * change) — every `stringResource()` call in the affected composables is live,
 * not cached, and `recreate()` throws away the whole composition — but this
 * test pins the guarantee down so a future regression here fails loudly
 * instead of requiring another manual investigation.
 *
 * Run with `./gradlew app:connectedDebugAndroidTest --tests
 * pl.kinowo.LanguageSwitchUpdatesRenderedCaptionsTest` (or via
 * `android/scripts/devtest.sh`).
 */
@RunWith(AndroidJUnit4::class)
class LanguageSwitchUpdatesRenderedCaptionsTest {

    // createEmptyComposeRule (rather than createAndroidComposeRule<MainActivity>)
    // so the language pref can be persisted BEFORE the activity is launched
    // manually below — mirroring LanguageSwitchKeepsRetainedViewModelTest. It
    // still gives the Compose test APIs (onNodeWithText, waitUntil, ...) against
    // whichever Compose hierarchy is currently active in the process.
    @get:Rule
    val compose = createEmptyComposeRule()

    // MainActivity requests approximate location on launch; pre-grant it so the
    // system dialog never steals focus (which would leave the activity paused
    // and ActivityScenario waiting for RESUMED).
    @Before
    fun grantLocation() {
        val pkg = ApplicationProvider.getApplicationContext<Context>().packageName
        InstrumentationRegistry.getInstrumentation().uiAutomation
            .grantRuntimePermission(pkg, Manifest.permission.ACCESS_COARSE_LOCATION)
    }

    @Test
    fun switchingLanguageRepaintsTheDatePillOnScreen() {
        val context = ApplicationProvider.getApplicationContext<Context>()
        val prefs = UserPreferences(context)
        // A fresh install has no selected city, which gates the list screen
        // behind CityChoiceScreen (see KinowoApp) — the date pills this test
        // asserts on live only past that gate.
        runBlocking { prefs.setCity("warszawa") }
        runBlocking { prefs.setLanguageTag("pl") }

        ActivityScenario.launch(MainActivity::class.java).use {
            compose.waitUntil(timeoutMillis = 15_000) {
                compose.onAllNodesWithText("Dziś").fetchSemanticsNodes().isNotEmpty()
            }

            // Switch to English. MainActivity observes the persisted tag and
            // recreate()s — the whole composition is thrown away and rebuilt,
            // so every stringResource() call must re-resolve against the new
            // locale from scratch.
            runBlocking { prefs.setLanguageTag("en") }

            compose.waitUntil(timeoutMillis = 15_000) {
                compose.onAllNodesWithText("Today").fetchSemanticsNodes().isNotEmpty()
            }
            compose.onNodeWithText("Today").assertExists(
                "the Dziś/Today date pill must repaint with the new language after the recreate()",
            )
        }
    }
}
