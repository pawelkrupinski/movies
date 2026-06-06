package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.filter.CinemaCityFilter

/**
 * `disabledCinemas` is one global set shared by every city. After a city
 * switch a cinema deselected elsewhere lingers in it; the count badge and the
 * "Wszystkie kina" toggle must be derived from the cinemas that belong to the
 * CURRENT city, else the count reads one short and the toggle wrongly shows
 * "not all selected" on arrival. These pin that scoping down.
 */
class CinemaCityFilterTest {

    // The "foreign" name belongs to another city; the rest are this city's.
    private val foreign = "Kino Pod Baranami"
    private val city = listOf("Kino Apollo", "Helios Posnania", "Multikino Stary Browar")

    @Test
    fun foreignDeselectionDoesNotAffectThisCity() {
        val disabled = setOf(foreign)
        assertEquals(emptySet<String>(), CinemaCityFilter.disabledIn(disabled, city))
        assertTrue(CinemaCityFilter.allSelected(disabled, city))
        // The pre-fix badge/toggle read the RAW set — non-empty here. That was
        // the bug; the scoped check above is the fix.
        assertTrue(disabled.isNotEmpty())
    }

    @Test
    fun inCityDeselectionScopesToTotalMinusOne() {
        val disabled = setOf(foreign, "Kino Apollo")
        assertEquals(setOf("Kino Apollo"), CinemaCityFilter.disabledIn(disabled, city))
        assertFalse(CinemaCityFilter.allSelected(disabled, city))
    }

    @Test
    fun selectAllDropsOnlyThisCityPreservingOthers() {
        val disabled = setOf(foreign, "Kino Apollo")
        val after = CinemaCityFilter.afterToggleAll(disabled, city, selected = true)
        assertEquals(setOf(foreign), after)
        assertTrue(CinemaCityFilter.allSelected(after, city))
    }

    @Test
    fun deselectAllAddsThisCityOnTopOfOthers() {
        val disabled = setOf(foreign)
        val after = CinemaCityFilter.afterToggleAll(disabled, city, selected = false)
        assertEquals(setOf(foreign) + city, after)
        assertFalse(CinemaCityFilter.allSelected(after, city))
    }
}
