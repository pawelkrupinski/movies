package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.deeplink.DeepLink
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.SortOption
import pl.kinowo.model.Film
import pl.kinowo.ui.KinowoViewModel

/**
 * Guards that a link's scalar filters actually land on the [KinowoViewModel]
 * state — the Android side of the deep-link wiring. The pure parse +
 * inclusion→exclusion conversion is covered by [DeepLinkTest]; here we exercise
 * the ViewModel's [KinowoViewModel.applyScalarFilters], which `handleDeepLink`
 * runs synchronously.
 *
 * The ViewModel comes from [KinowoViewModelHarness], which also tears it
 * down so no async prefs write outlives the test.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class DeepLinkApplyTest {

    @get:Rule
    val harness = KinowoViewModelHarness()

    @Test
    fun scalarFiltersFromLinkLandOnViewModelState() {
        val vm = harness.viewModel()

        vm.applyScalarFilters(DeepLink.parse("https://kinowo.net/warszawa/?date=tomorrow&q=duna&dim=2D&lang=NAP&imax=1&from=18:30&sort=rating")!!.filters)

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
        val vm = harness.viewModel()

        // A link that sets only `dim` must not disturb the other axes.
        vm.applyScalarFilters(DeepLink.parse("https://kinowo.net/warszawa/?dim=3D")!!.filters)

        assertEquals("3D", vm.formatFilter.dimension)
        assertEquals(DateFilter.Today, vm.dateFilter)
        assertEquals("", vm.search)
        assertEquals(SortOption.DEFAULT, vm.sortBy)
    }

    @Test
    fun filmLinkQueuesNavigationWhenTitleIsInTheLoadedRepertoire() {
        val vm = harness.viewModel()

        // The film lookup runs against the list it's GIVEN — which the fix
        // guarantees is the target city's repertoire, not stale films.
        vm.applyRepertoireDependent(
            DeepLink.parse("https://kinowo.net/warszawa/movie?title=Wicked")!!,
            listOf(Film(title = "Other film"), Film(title = "Wicked")),
        )

        assertEquals("Wicked", vm.pendingFilmNav)
    }

    @Test
    fun filmLinkMatchesNumberedTitleByNormalizedForm() {
        val vm = harness.viewModel()

        // The web links "…Prady 2" (Arabic, as displayed) but the stored film is
        // "…Prady II" — exact match would miss. Normalized match finds it, and we
        // navigate with the FOUND film's real title so the detail route resolves.
        vm.applyRepertoireDependent(
            DeepLink.parse("https://kinowo.net/warszawa/movie?title=Diabe%C5%82%20ubiera%20si%C4%99%20u%20Prady%202")!!,
            listOf(Film(title = "Diabeł ubiera się u Prady II")),
        )

        assertEquals("Diabeł ubiera się u Prady II", vm.pendingFilmNav)
    }

    @Test
    fun filmLinkDoesNotNavigateWhenTitleAbsentFromRepertoire() {
        val vm = harness.viewModel()

        // Title not in the supplied list (e.g. matched against the wrong/stale
        // city, or the film left the listing) → no navigation, lands on the grid.
        vm.applyRepertoireDependent(
            DeepLink.parse("https://kinowo.net/warszawa/movie?title=Wicked")!!,
            listOf(Film(title = "Other film")),
        )

        assertNull(vm.pendingFilmNav)
    }
}
