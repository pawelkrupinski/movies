package pl.kinowo.ui

import android.os.Looper
import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import org.junit.Assert.assertFalse
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.Shadows.shadowOf
import org.robolectric.annotation.Config
import pl.kinowo.auth.AuthRepository
import pl.kinowo.auth.HiddenFilmsClient
import pl.kinowo.auth.HiddenFilmsFetchResult
import pl.kinowo.auth.HiddenFilmsState
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.PersistentCookieJar

/**
 * [KinowoViewModel.deleteAccount] wipes local hiddenFilms/disabledCinemas, but
 * used to leave every country's `hiddenFilmsMigrated`/validators flags behind
 * — the same omission the old single-flag `serverStateSynced` design had
 * before it. A stale "migrated" flag surviving into the NEXT login (same
 * device, fresh account) would make `StateSyncService.reconcile` treat that
 * country as already synced, skip the union, and mirror back whatever the
 * fresh account's empty server row has — silently dropping anything the new
 * session-holder set locally before signing back in.
 *
 * `AuthRepository.deleteAccount()`'s own network call is wrapped in
 * `runCatching` (never throws), so a real instance pointed at an unreachable
 * URL in this JVM test still reaches `clearSession()` and returns — no
 * MockWebServer stub needed to exercise the rest of the method.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class DeleteAccountClearsSyncStateTest {

    private fun viewModel(prefs: UserPreferences): KinowoViewModel {
        val context = ApplicationProvider.getApplicationContext<android.content.Context>()
        val http = OkHttpClient()
        val api = KinowoApi(client = http)
        val repository = RepertoireRepository(api, JsonListCache(context.cacheDir, "repertoire", Film.serializer()))
        val detailsRepository = DetailsRepository(api, JsonListCache(context.cacheDir, "details", FilmDetails.serializer()))
        val authRepository = AuthRepository(http, PersistentCookieJar(context))
        val noopStateClient = object : HiddenFilmsClient {
            override suspend fun fetch(country: String, etag: String?, lastModified: String?) =
                HiddenFilmsFetchResult.NotModified
            override suspend fun hide(country: String, title: String) = HiddenFilmsState(emptySet(), null, null)
            override suspend fun unhide(country: String, title: String) = HiddenFilmsState(emptySet(), null, null)
            override suspend fun clear(country: String) = HiddenFilmsState(emptySet(), null, null)
        }
        return KinowoViewModel(repository, detailsRepository, prefs, authRepository, noopStateClient)
    }

    @Test
    fun deleteAccountClearsThePerCountryHiddenFilmsMigrationFlag() {
        val context = ApplicationProvider.getApplicationContext<android.content.Context>()
        val prefs = UserPreferences(context)
        runBlocking {
            prefs.setHiddenFilmsMigrated("pl", true)
            prefs.setHiddenFilmsValidators("pl", "\"etag\"", "Tue, 19 May 2026 12:00:00 GMT")
        }
        val vm = viewModel(prefs)

        // `deleteAccount()`'s own `authRepository.deleteAccount()` hops onto a
        // REAL IO thread (`withContext(Dispatchers.IO)`) before resuming back
        // on the (Robolectric-paused) Main dispatcher `viewModelScope` uses —
        // `.join()` alone deadlocks here, since nothing pumps Main while it
        // waits. Poll instead: idle Main to let the coroutine start and
        // dispatch to IO, then keep idling (picking up the resumption once
        // the real background hop completes) until the flag actually clears.
        vm.deleteAccount()
        val deadline = System.currentTimeMillis() + 5_000
        while (System.currentTimeMillis() < deadline) {
            shadowOf(Looper.getMainLooper()).idle()
            if (!runBlocking { prefs.isHiddenFilmsMigrated("pl") }) break
            Thread.sleep(10)
        }

        runBlocking {
            assertFalse(
                "deleteAccount() must clear the per-country migration flag, or the next " +
                    "login (a different account on this device) would wrongly treat 'pl' as already synced",
                prefs.isHiddenFilmsMigrated("pl"),
            )
        }
    }
}
