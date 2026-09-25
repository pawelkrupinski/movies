package pl.kinowo.ui

import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.KinowoViewModelHarness
import pl.kinowo.auth.StateSync
import pl.kinowo.data.UserPreferences
import java.util.concurrent.CopyOnWriteArrayList

/**
 * [KinowoViewModel] drives the server mirror through the [StateSync] it is
 * handed — it starts it, reconciles through it on foreground-resume, and routes
 * each local edit to it with the country the edit went to. The sync used to be
 * built inside the ViewModel, so none of this was observable without the real,
 * network-facing [pl.kinowo.auth.StateSyncService].
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class ViewModelDrivesStateSyncTest {

    @get:Rule
    val harness = KinowoViewModelHarness()

    private class RecordingStateSync : StateSync {
        val calls = CopyOnWriteArrayList<String>()
        override fun start() { calls += "start" }
        override suspend fun reconcileCurrentCountry() { calls += "reconcile" }
        override fun hide(country: String, title: String) { calls += "hide $country $title" }
        override fun unhide(country: String, title: String) { calls += "unhide $country $title" }
        override fun clear(country: String) { calls += "clear $country" }
    }

    @Test
    fun constructionStartsTheInjectedSyncOnce() {
        val sync = RecordingStateSync()

        harness.viewModel(sync = sync)

        assertEquals(listOf("start"), sync.calls)
    }

    @Test
    fun onResumeReconcilesThroughTheInjectedSync() {
        val sync = RecordingStateSync()
        val vm = harness.viewModel(sync = sync)

        vm.onResume()

        harness.pumpUntil("onResume to reconcile") { "reconcile" in sync.calls }
        assertEquals(listOf("start", "reconcile"), sync.calls)
    }

    @Test
    fun localEditsArePushedWithTheCountryTheyWereMadeIn() {
        val prefs = UserPreferences(harness.context)
        runBlocking { prefs.setCountryCode("uk") }
        val sync = RecordingStateSync()
        val vm = harness.viewModel(prefs = prefs, sync = sync)

        harness.settle(vm.hide("Dune"))
        harness.settle(vm.unhide("Dune"))
        harness.settle(vm.unhideAll())

        assertEquals(listOf("start", "hide uk Dune", "unhide uk Dune", "clear uk"), sync.calls)
    }
}
