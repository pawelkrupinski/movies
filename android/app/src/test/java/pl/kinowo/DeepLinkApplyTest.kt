package pl.kinowo

import androidx.test.core.app.ApplicationProvider
import okhttp3.OkHttpClient
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
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
import pl.kinowo.deeplink.DeepLink
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.SortOption
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.PersistentCookieJar
import pl.kinowo.ui.KinowoViewModel

/**
 * Guards that a link's scalar filters actually land on the [KinowoViewModel]
 * state — the Android side of the deep-link wiring. The pure parse +
 * inclusion→exclusion conversion is covered by [DeepLinkTest]; here we exercise
 * the ViewModel's [KinowoViewModel.applyScalarFilters], which `handleDeepLink`
 * runs synchronously.
 *
 * We assert on `applyScalarFilters` rather than the whole `handleDeepLink`
 * on purpose: `handleDeepLink` also calls `setCity`, which WRITES to the
 * process-shared Robolectric DataStore (`kinowo_prefs`). That write — async on
 * `viewModelScope` — outlives the test and wedges the shared write-mutex,
 * deadlocking the next test that reads prefs (`UserPreferencesCityTest`). Reads
 * are fine, writes are not; staying read-only keeps the suite hermetic.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class DeepLinkApplyTest {

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
        return KinowoViewModel(repository, detailsRepository, UserPreferences(context), authRepository, noopStateClient)
    }

    @Test
    fun scalarFiltersFromLinkLandOnViewModelState() {
        val vm = viewModel()

        vm.applyScalarFilters(DeepLink.parse("https://kinowo.fly.dev/warszawa/?date=tomorrow&q=duna&dim=2D&lang=NAP&imax=1&from=18:30&sort=rating")!!.filters)

        assertEquals(DateFilter.Tomorrow, vm.dateFilter)
        assertEquals("duna", vm.search)
        assertEquals("2D", vm.formatFilter.dimension)
        assertEquals("NAP", vm.formatFilter.language)
        assertEquals(true, vm.formatFilter.imax)
        assertEquals(18, vm.formatFilter.fromHour)
        assertEquals(30, vm.formatFilter.fromMinute)
        assertEquals(SortOption.RATING, vm.sortBy)
    }

    @Test
    fun absentAxesLeaveStateAtDefaults() {
        val vm = viewModel()

        // A link that sets only `dim` must not disturb the other axes.
        vm.applyScalarFilters(DeepLink.parse("https://kinowo.fly.dev/warszawa/?dim=3D")!!.filters)

        assertEquals("3D", vm.formatFilter.dimension)
        assertEquals(DateFilter.Today, vm.dateFilter)
        assertEquals("", vm.search)
        assertEquals(SortOption.DEFAULT, vm.sortBy)
    }

    @Test
    fun filmLinkQueuesNavigationWhenTitleIsInTheLoadedRepertoire() {
        val vm = viewModel()

        // The film lookup runs against the list it's GIVEN — which the fix
        // guarantees is the target city's repertoire, not stale films.
        vm.applyRepertoireDependent(
            DeepLink.parse("https://kinowo.fly.dev/warszawa/film?title=Wicked")!!,
            listOf(Film(title = "Other film"), Film(title = "Wicked")),
        )

        assertEquals("Wicked", vm.pendingFilmNav)
    }

    @Test
    fun filmLinkMatchesNumberedTitleByNormalizedForm() {
        val vm = viewModel()

        // The web links "…Prady 2" (Arabic, as displayed) but the stored film is
        // "…Prady II" — exact match would miss. Normalized match finds it, and we
        // navigate with the FOUND film's real title so the detail route resolves.
        vm.applyRepertoireDependent(
            DeepLink.parse("https://kinowo.fly.dev/warszawa/film?title=Diabe%C5%82%20ubiera%20si%C4%99%20u%20Prady%202")!!,
            listOf(Film(title = "Diabeł ubiera się u Prady II")),
        )

        assertEquals("Diabeł ubiera się u Prady II", vm.pendingFilmNav)
    }

    @Test
    fun filmLinkDoesNotNavigateWhenTitleAbsentFromRepertoire() {
        val vm = viewModel()

        // Title not in the supplied list (e.g. matched against the wrong/stale
        // city, or the film left the listing) → no navigation, lands on the grid.
        vm.applyRepertoireDependent(
            DeepLink.parse("https://kinowo.fly.dev/warszawa/film?title=Wicked")!!,
            listOf(Film(title = "Other film")),
        )

        assertNull(vm.pendingFilmNav)
    }
}
