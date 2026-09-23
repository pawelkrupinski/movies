package pl.kinowo.data

import android.content.Context
import android.os.Looper
import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.runBlocking
import org.junit.rules.ExternalResource
import org.robolectric.Shadows.shadowOf
import java.util.concurrent.atomic.AtomicReference

/**
 * Hands each test an EMPTY [UserPreferences] and leaves it empty afterwards.
 *
 * The `kinowo_prefs` DataStore is a process singleton, so under Robolectric
 * every test class in a Gradle fork shares one store: without this, a test
 * asserting "null until set" passes or fails depending on which test happened
 * to write before it.
 *
 * Write through [write] whenever app code may be writing too (a live
 * ViewModel or activity). DataStore runs an `edit` transform in the CALLER's
 * context while holding its process-wide lock, so an app write launched on
 * Main parks the lock until the main Looper runs it: a `runBlocking` write on
 * the (Robolectric main) test thread then deadlocks, and a transform still
 * queued when the test ends is discarded with the Looper — wedging the lock
 * for every later test in the fork. [write] runs off-thread and pumps Main
 * until done; the teardown wipe goes through it, so it also proves the lock
 * free before the next test starts.
 */
class FreshUserPreferences : ExternalResource() {

    val context: Context get() = ApplicationProvider.getApplicationContext()
    val prefs: UserPreferences get() = UserPreferences(context)

    public override fun before() = write { clearAllForTest() }
    public override fun after() = write { clearAllForTest() }

    fun write(block: suspend UserPreferences.() -> Unit) {
        val prefs = prefs
        val outcome = AtomicReference<Result<Unit>?>(null)
        Thread { outcome.set(runCatching { runBlocking { prefs.block() } }) }.apply { isDaemon = true }.start()
        val deadline = System.currentTimeMillis() + TIMEOUT_MS
        while (outcome.get() == null) {
            check(System.currentTimeMillis() < deadline) { "DataStore write still blocked after ${TIMEOUT_MS}ms" }
            shadowOf(Looper.getMainLooper()).idle()
            Thread.sleep(5)
        }
        outcome.get()!!.getOrThrow()
    }

    private companion object {
        const val TIMEOUT_MS = 5_000L
    }
}
