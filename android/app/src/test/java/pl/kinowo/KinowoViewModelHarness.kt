package pl.kinowo

import android.content.Context
import android.os.Looper
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.ViewModelStore
import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.Job
import okhttp3.OkHttpClient
import org.junit.rules.ExternalResource
import org.robolectric.Shadows.shadowOf
import pl.kinowo.auth.StateSync
import pl.kinowo.data.FreshUserPreferences
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.location.GrantedLocationSource
import pl.kinowo.ui.KinowoViewModel

/**
 * Builds real [KinowoViewModel]s for Robolectric tests AND tears them down —
 * the part every copy-pasted `viewModel()` helper used to skip, which is what
 * hung the test JVM.
 *
 * The mechanism: DataStore runs an `edit {}` transform in the CALLER's
 * coroutine context while holding its process-wide write lock. A ViewModel
 * write launched on `viewModelScope` (Main) therefore posts that transform to
 * Robolectric's main Looper — and Robolectric discards the Looper's queue at
 * the end of each test. A write still pending then never runs, never releases
 * the lock, and the `kinowo_prefs` DataStore is a process singleton, so the
 * NEXT test to write anything (in any class sharing the fork) parks forever at
 * 0% CPU. That is exactly how `CitySwitchSuppressionWiringTest` (whose
 * `chooseCityAtGate` write was never awaited) wedged
 * `DeleteAccountClearsSyncStateTest`'s very first `setHiddenFilmsMigrated`.
 *
 * So: build ViewModels through [viewModel] (owned by a [ViewModelStore] that
 * [after] clears, cancelling every `viewModelScope` collector), await the
 * write-launching calls you make with [settle], and let [after] prove the
 * DataStore lock is free before the next test starts.
 *
 * The ViewModel itself is [testKinowoViewModel]'s: offline, never reaching prod.
 */
class KinowoViewModelHarness : ExternalResource() {

    val context: Context get() = ApplicationProvider.getApplicationContext()

    private val stores = mutableListOf<ViewModelStore>()
    private val clients = mutableListOf<OkHttpClient>()

    fun viewModel(
        prefs: UserPreferences = UserPreferences(context),
        repository: RepertoireRepository? = null,
        sync: StateSync = NoopStateSync,
        location: GrantedLocationSource = GrantedLocationSource { null },
    ): KinowoViewModel {
        val http = OkHttpClient().also { clients += it }
        val factory = object : ViewModelProvider.Factory {
            @Suppress("UNCHECKED_CAST")
            override fun <T : ViewModel> create(modelClass: Class<T>): T =
                testKinowoViewModel(context, repository, prefs, sync, location, http) as T
        }
        val store = ViewModelStore().also { stores += it }
        return ViewModelProvider(store, factory)[KinowoViewModel::class.java]
    }

    /** Pump the main Looper until [job] — a `viewModelScope.launch` a
     *  ViewModel method returned — completes. `job.join()` alone would
     *  deadlock: the job resumes ON the main Looper this thread must pump. */
    fun settle(job: Job) {
        pumpUntil("the ViewModel job to complete") { job.isCompleted }
    }

    // Every test starts and ends on an empty prefs store (see FreshUserPreferences).
    private val fresh = FreshUserPreferences()

    override fun before() = fresh.before()

    override fun after() {
        stores.forEach { it.clear() }
        stores.clear()
        fresh.after() // also proves the DataStore write lock free (see FreshUserPreferences)
        clients.forEach { it.dispatcher.executorService.shutdown(); it.connectionPool.evictAll() }
        clients.clear()
    }

    /** Keep pumping for [millis] — for asserting something did NOT happen. */
    fun pumpFor(millis: Long) {
        val end = System.currentTimeMillis() + millis
        pumpUntil("$millis ms to pass") { System.currentTimeMillis() >= end }
    }

    fun pumpUntil(what: String, condition: () -> Boolean) {
        val deadline = System.currentTimeMillis() + TIMEOUT_MS
        while (!condition()) {
            check(System.currentTimeMillis() < deadline) { "Timed out after ${TIMEOUT_MS}ms waiting for $what" }
            shadowOf(Looper.getMainLooper()).idle()
            Thread.sleep(5)
        }
    }

    private companion object {
        const val TIMEOUT_MS = 5_000L
    }
}
