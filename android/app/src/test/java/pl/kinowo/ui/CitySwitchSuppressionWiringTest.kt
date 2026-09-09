package pl.kinowo.ui

import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
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
 * [KinowoViewModel.chooseCityAtGate] must not let [KinowoViewModel.checkCitySwitch]
 * immediately re-offer the "you're nearer …" prompt for a city the user just
 * picked on purpose — first-launch or a later re-pick alike.
 *
 * The first-launch path (a real detected [nearestSlug]) already had a precise
 * fix: it seeds [pl.kinowo.data.UserPreferences.citySwitchPromptKey] with the
 * exact `chosen→nearest` pair, which [pl.kinowo.model.switchSuggestion] then
 * recognises and stays quiet for. A later re-pick — Filtry's "Pick another
 * city", or a country switch — has no detected nearest to build that pair
 * from ([nearestSlug] is null), so it falls back to the SAME one-shot
 * suppressor a web sign-in's Custom Tab resume already uses: skip the very
 * next [checkCitySwitch], not check-by-key.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class CitySwitchSuppressionWiringTest {

    private fun viewModel(): KinowoViewModel {
        val context = ApplicationProvider.getApplicationContext<android.content.Context>()
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
    fun rePickingWithNoDetectedNearestArmsTheBlanketSuppressor() {
        val vm = viewModel()

        runBlocking { vm.chooseCityAtGate("warszawa", nearestSlug = null) }

        assertTrue(
            "chooseCityAtGate(nearestSlug = null) should skip the next checkCitySwitch",
            vm.citySwitchSuppressor.consumeShouldSkip(),
        )
    }

    @Test
    fun firstLaunchWithADetectedNearestUsesThePreciseKeyInstead() {
        val vm = viewModel()

        runBlocking { vm.chooseCityAtGate("warszawa", nearestSlug = "poznan") }

        // The exact chosen→nearest pair is handled by the persisted prompt key
        // (see UserPreferencesCityTests-equivalent coverage of setCitySwitchPromptKey);
        // the blanket suppressor has nothing to do here and stays disarmed.
        assertFalse(
            "A real detected nearest should be handled by the precise key, not the blanket suppressor",
            vm.citySwitchSuppressor.consumeShouldSkip(),
        )
    }

    @Test
    fun rePickingTheAlreadyNearestCityAlsoLeavesTheBlanketSuppressorDisarmed() {
        val vm = viewModel()

        // chosen == nearest: initialChoiceSuppressKey returns null (nothing to
        // suppress — switchSuggestion already stays quiet when nearest equals
        // chosen), and nearestSlug is non-null, so the blanket fallback must not
        // fire either.
        runBlocking { vm.chooseCityAtGate("warszawa", nearestSlug = "warszawa") }

        assertFalse(vm.citySwitchSuppressor.consumeShouldSkip())
    }
}
