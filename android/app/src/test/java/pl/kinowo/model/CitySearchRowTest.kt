package pl.kinowo.model

import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * [searchRows] flattens every region, subregion and city into ONE
 * alphabetical list, so a query matches whichever level it names — unlike
 * [topLevelRows]/[secondLevelRows] ([CityPickerRowTest]), which only ever
 * look at the level they're building. Mirrors iOS's `CitySearchRowsTests`.
 */
class CitySearchRowTest {

    private val birmingham = City("birmingham", "Birmingham", 52.46, -1.9, "uk", "England", "West Midlands")
    private val dudley = City("dudley", "Dudley", 52.5, -2.09, "uk", "England", "West Midlands")
    private val cheshire = City("cheshire", "Cheshire", 53.29, -2.5, "uk", "England")
    private val glasgow = City("glasgow", "Glasgow", 55.86, -4.25, "uk", "Scotland")

    private val cities = listOf(birmingham, dudley, cheshire, glasgow)

    @Test
    fun aSubregionTwoLevelsDeepIsFound() {
        assertEquals(
            listOf(CitySearchRow.Subregion("West Midlands", region = "England")),
            cities.searchRows("west midlands", "uk"),
        )
    }

    @Test
    fun aRegionNameMatchesEvenWithoutDrillingIntoIt() {
        assertEquals(listOf(CitySearchRow.Region("Scotland")), cities.searchRows("scotland", "uk"))
    }

    @Test
    fun aCityNameStillMatchesRegardlessOfNesting() {
        assertEquals(listOf(CitySearchRow.CityRow(glasgow)), cities.searchRows("glasgow", "uk"))
    }

    /**
     * Matches from every level land in ONE alphabetically sorted list — a
     * city ("Cheshire", "Dudley"), a region ("England") and a subregion
     * ("West Midlands") all interleave by NAME here, not grouped by kind or
     * by depth (Birmingham and Glasgow carry no "e", so they drop out).
     */
    @Test
    fun matchesFromDifferentLevelsSortTogetherAlphabetically() {
        assertEquals(
            listOf("Cheshire", "Dudley", "England", "West Midlands"),
            cities.searchRows("e", "uk").map { it.label },
        )
    }

    @Test
    fun blankQueryYieldsNothingSincePerLevelRowsAlreadyCoverIt() {
        assertEquals(emptyList<CitySearchRow>(), cities.searchRows("", "uk"))
        assertEquals(emptyList<CitySearchRow>(), cities.searchRows("   ", "uk"))
    }

    @Test
    fun noMatchYieldsAnEmptyList() {
        assertEquals(emptyList<CitySearchRow>(), cities.searchRows("zzz", "uk"))
    }

    @Test
    fun foldsDiacriticsInRegionNamesToo() {
        val lodz = City("lodz", "Łódź", 51.77, 19.46, "pl", "Łódzkie")
        assertEquals(listOf(CitySearchRow.Region("Łódzkie")), listOf(lodz).searchRows("lodzkie", "pl"))
    }

    /** A place whose state collapsed onto it alone keeps that state's name as
     *  an alias, and every search answers to it the way the web's `data-alias`
     *  does: "guam" must find the row that reads "Dededo Village area". */
    @Test
    fun aCollapsedCityIsFoundByItsGroupsName() {
        val dededo = City("dededo-village", "Dededo Village area", 13.5, 144.8, "us", aliases = listOf("Guam"))
        val austin = City("austin", "Austin", 30.27, -97.74, "us", "Texas")
        val us = listOf(dededo, austin)
        assertEquals(listOf(CitySearchRow.CityRow(dededo)), us.searchRows("guam", "us"))
        assertEquals(listOf(CityPickerRow.CityRow(dededo)), us.topLevelRows("guam", "us"))
        assertEquals(listOf(dededo), us.matching("guam", "us"))
    }
}
