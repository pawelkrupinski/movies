package pl.kinowo.data

import android.content.Context
import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.runBlocking
import org.junit.rules.ExternalResource

/**
 * Hands each test an EMPTY [UserPreferences] and leaves it empty afterwards.
 *
 * The `kinowo_prefs` DataStore is a process singleton, so under Robolectric
 * every test class in a Gradle fork shares one store: without this, a test
 * asserting "null until set" passes or fails depending on which test happened
 * to write before it.
 */
class FreshUserPreferences : ExternalResource() {

    val context: Context get() = ApplicationProvider.getApplicationContext()
    val prefs: UserPreferences get() = UserPreferences(context)

    public override fun before() = wipe()
    public override fun after() = wipe()

    private fun wipe() {
        runBlocking { prefs.clearAllForTest() }
    }
}
