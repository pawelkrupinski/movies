package pl.kinowo.ui

import android.content.Context
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.auth.AuthRepository
import pl.kinowo.auth.UserStateClient
import pl.kinowo.auth.UserSyncState
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.PersistentCookieJar

/**
 * A manual re-pick with no detected nearest (Filtry's "Pick another city", or
 * a country switch) is only guarded by [KinowoViewModel.citySwitchSuppressor],
 * a ONE-SHOT flag meant to skip exactly the single [KinowoViewModel.checkCitySwitch]
 * call [KinowoViewModel.chooseCityAtGate]'s doc calls "the ONE check [it] fires
 * right after this pick". But [NearerCityPrompt] wires TWO triggers —
 * `LaunchedEffect(Unit)` (on entry) and `LifecycleEventEffect(ON_RESUME)` — and
 * Android's Lifecycle synchronously replays `ON_RESUME` to an observer added
 * while the owner is already resumed, which it always is right after a manual
 * pick (no real pause/resume happened — only the composed content changed). So
 * both fire on the very same composition: the first consumes the suppressor and
 * stays quiet, but the SECOND finds it already spent and proceeds into a real
 * check — which is exactly how "you're nearer Poznań" can surface right after a
 * manual pick.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class NearerCityPromptDoubleCheckTest {

    @get:Rule
    val compose = createComposeRule()

    private fun viewModel(): KinowoViewModel {
        val context = ApplicationProvider.getApplicationContext<Context>()
        val http = OkHttpClient()
        val api = KinowoApi(client = http)
        val repository = RepertoireRepository(api, JsonListCache(context.cacheDir, "repertoire", Film.serializer()))
        val detailsRepository = DetailsRepository(api, JsonListCache(context.cacheDir, "details", FilmDetails.serializer()))
        val authRepository = AuthRepository(http, PersistentCookieJar(context))
        val noopStateClient = object : UserStateClient {
            override suspend fun fetchState() = UserSyncState(emptySet(), emptySet())
            override suspend fun putState(state: UserSyncState) {}
        }
        val prefs = UserPreferences(context)
        return KinowoViewModel(repository, detailsRepository, prefs, authRepository, noopStateClient)
    }

    @Test
    fun mountingRightAfterAManualRepickChecksExactlyOnce() {
        val vm = viewModel()
        // The "no detected nearest" branch of chooseCityAtGate — Filtry's "Pick
        // another city", or a country switch — is exactly the one that relies on
        // the fragile one-shot suppressor rather than a persisted chosen→nearest
        // key.
        runBlocking { vm.chooseCityAtGate("warszawa", nearestSlug = null) }

        // Mirrors KinowoApp: NearerCityPrompt mounts once `selectedCity` is
        // non-null, which happens right after the pick above — no real
        // pause/resume of the host occurs in between.
        compose.setContent { NearerCityPrompt(vm) }
        compose.waitForIdle()

        assertEquals(
            "the mount-time LaunchedEffect and the ON_RESUME catch-up dispatch " +
                "both fire for one manual pick, but only ONE check is meant to run " +
                "per chooseCityAtGate's own doc comment",
            1,
            vm.checkCitySwitchInvocationCount,
        )
    }
}
