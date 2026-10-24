package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.filter.DateFilter

/** The search's day through the ViewModel the screens drive: the search box and the day it shows
 *  ([KinowoViewModel.search], [KinowoViewModel.dateFilter], as a pill tap or a swipe sets it). */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class SearchDayViewModelTest {

    @get:Rule
    val harness = KinowoViewModelHarness()

    @Test
    fun aSearchShowsEveryDayAndClearingGoesBack() {
        val vm = harness.viewModel()
        vm.dateFilter = DateFilter.Tomorrow
        vm.search = "s"
        assertEquals(DateFilter.Anytime, vm.dateFilter)
        assertNotNull("the All page draws the added days lighter", vm.outsideDays(DateFilter.Anytime))
        assertNull(vm.outsideDays(DateFilter.Tomorrow))
        vm.search = "st"
        vm.search = ""
        assertEquals(DateFilter.Tomorrow, vm.dateFilter)
        assertNull(vm.outsideDays(DateFilter.Anytime))
    }

    @Test
    fun aDayPickedDuringTheSearchStaysWhenItIsCleared() {
        val vm = harness.viewModel()
        vm.search = "dune"
        vm.dateFilter = DateFilter.Tomorrow
        vm.dateFilter = DateFilter.Week
        assertNull("a picked day draws nothing lighter", vm.outsideDays(DateFilter.Anytime))
        vm.search = ""
        assertEquals(DateFilter.Week, vm.dateFilter)
    }
}
