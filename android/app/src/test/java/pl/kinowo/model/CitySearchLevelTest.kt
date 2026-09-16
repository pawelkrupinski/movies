package pl.kinowo.model

import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * [searchLevelsFor] picks the search box's contextual copy — mirrors
 * `pickerSearchLevels()` in landing.scala.html, so these pin the same cases
 * that file's own comment calls out: a nesting country shows two levels at
 * its root and one once a region is picked, a single-level country shows
 * one, and a flat country shows none.
 */
class CitySearchLevelTest {

    @Test
    fun theUkShowsBothLevelsAtItsRoot() {
        assertEquals(
            listOf(CitySearchLevel.UkRegion, CitySearchLevel.UkSubregion),
            searchLevelsFor("uk", region = null, subregion = null),
        )
    }

    @Test
    fun theUkDropsToItsSecondLevelOnceARegionIsPicked() {
        assertEquals(
            listOf(CitySearchLevel.UkSubregion),
            searchLevelsFor("uk", region = "England", subregion = null),
        )
    }

    @Test
    fun theUkReachesNoFurtherLevelOnceFullyDrilled() {
        assertEquals(
            emptyList<CitySearchLevel>(),
            searchLevelsFor("uk", region = "England", subregion = "West Midlands"),
        )
    }

    @Test
    fun aSingleLevelCountryShowsOnlyItsOwnTerm() {
        assertEquals(listOf(CitySearchLevel.UsRegion), searchLevelsFor("us", region = null, subregion = null))
        assertEquals(listOf(CitySearchLevel.DeRegion), searchLevelsFor("de", region = null, subregion = null))
    }

    @Test
    fun aSingleLevelCountryReachesNoFurtherLevelOnceARegionIsPicked() {
        assertEquals(emptyList<CitySearchLevel>(), searchLevelsFor("us", region = "California", subregion = null))
    }

    @Test
    fun aFlatCountryNeverShowsAGroupTerm() {
        assertEquals(emptyList<CitySearchLevel>(), searchLevelsFor("pl", region = null, subregion = null))
        assertEquals(emptyList<CitySearchLevel>(), searchLevelsFor("es", region = null, subregion = null))
    }
}
