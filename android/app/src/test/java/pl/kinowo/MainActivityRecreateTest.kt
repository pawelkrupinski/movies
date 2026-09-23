package pl.kinowo

import android.app.Activity
import android.content.Intent
import androidx.compose.ui.test.junit4.createEmptyComposeRule
import androidx.test.core.app.ActivityScenario
import androidx.test.core.app.ApplicationProvider
import org.junit.Assert.assertSame
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.data.FreshUserPreferences

/**
 * MainActivity recreates itself when the language pick changes — and ONLY
 * then. Its watcher mapped DataStore's whole-store flow to the language key
 * without `distinctUntilChanged`, so EVERY prefs write (a hidden film, a city
 * pick, the swipe hint) re-emitted the same tag and recreated the activity.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], application = OfflineKinowoApplication::class)
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class MainActivityRecreateTest {

    @get:Rule(order = 0)
    val fresh = FreshUserPreferences()

    @get:Rule(order = 1)
    val compose = createEmptyComposeRule()

    @Test
    fun anUnrelatedPrefsWriteDoesNotRecreateTheActivity() {
        fresh.write { setCityInCountry("warszawa", "pl") }
        ActivityScenario.launch<MainActivity>(
            Intent(ApplicationProvider.getApplicationContext(), MainActivity::class.java),
        ).use { scenario ->
            compose.waitForIdle()
            lateinit var before: Activity
            scenario.onActivity { before = it }

            fresh.write { markSwiped() }
            val end = System.currentTimeMillis() + 500
            while (System.currentTimeMillis() < end) { compose.waitForIdle(); Thread.sleep(20) }

            lateinit var after: Activity
            scenario.onActivity { after = it }
            assertSame("a swipe-hint write must not recreate the activity", before, after)
        }
    }
}
