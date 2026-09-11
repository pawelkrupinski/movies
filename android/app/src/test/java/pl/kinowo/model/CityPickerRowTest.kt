package pl.kinowo.model

import org.junit.Assert.assertEquals
import org.junit.Test

/**
 * [topLevelRows] / [secondLevelRows] — the merge that interleaves a picker
 * level's group headings with its direct cities in ONE catalog-ordered pass,
 * rather than listing every heading before every direct city (the shape that
 * stranded "West Midlands" ahead of England's alphabetical list instead of
 * beside "West Sussex"). Mirrors iOS's `CityRegionTests`/`CitySubregionTests`.
 */
class CityPickerRowTest {

    private val losAngeles = City("los-angeles", "Los Angeles", 34.05, -118.24, "us", "California")
    private val sanDiego = City("san-diego", "San Diego", 32.72, -117.16, "us", "California")
    private val austin = City("austin", "Austin", 30.27, -97.74, "us", "Texas")
    private val poznan = City("poznan", "Poznań", 52.41, 16.93, "pl")

    private val cities = listOf(losAngeles, sanDiego, austin, poznan)

    @Test
    fun topLevelRowsAreDistinctHeadingsInCatalogOrder() {
        assertEquals(
            listOf(CityPickerRow.Heading("California"), CityPickerRow.Heading("Texas")),
            cities.topLevelRows("", "us"),
        )
    }

    @Test
    fun anUngroupedCountryReturnsPlainCityRows() {
        assertEquals(listOf(CityPickerRow.CityRow(poznan)), cities.topLevelRows("", "pl"))
    }

    @Test
    fun topLevelRowSearchFoldsLikeCityNamesDo() {
        assertEquals(listOf(CityPickerRow.Heading("California")), cities.topLevelRows("calif", "us"))
        assertEquals(listOf(CityPickerRow.Heading("Texas")), cities.topLevelRows("TEX", "us"))
        assertEquals(emptyList<CityPickerRow>(), cities.topLevelRows("zzz", "us"))
    }

    /**
     * A city whose top group collapsed onto it alone (Delaware, Vermont — too
     * small to split into metros) sits INTERLEAVED with the region headings,
     * at its own position in the catalog's order — not stranded after every
     * heading. Fails on the old "every heading, then every direct city"
     * shape, which put "West Midlands" ahead of its alphabetical neighbours
     * on the UK picker.
     */
    @Test
    fun aTopLevelDirectCityIsInterleavedWithHeadingsNotStrandedAfterThem() {
        val delaware = City("delaware", "Delaware", 39.0, -75.5, "us")
        // Catalog order: California's first city, then Delaware (no region —
        // its top group collapsed), then Texas's first city.
        val ordered = listOf(losAngeles, sanDiego, delaware, austin)
        assertEquals(
            listOf(CityPickerRow.Heading("California"), CityPickerRow.CityRow(delaware), CityPickerRow.Heading("Texas")),
            ordered.topLevelRows("", "us"),
        )
    }

    // ── secondLevelRows — the UK's West Midlands / Glamorgan / Antrim shape ──

    private val birmingham = City("birmingham", "Birmingham", 52.46, -1.9, "uk", "England", "West Midlands")
    private val dudley = City("dudley", "Dudley", 52.5, -2.09, "uk", "England", "West Midlands")
    private val cheshire = City("cheshire", "Cheshire", 53.29, -2.5, "uk", "England")
    private val london = City("london", "London", 51.51, -0.13, "uk", "England")

    private val ukCities = listOf(birmingham, dudley, cheshire, london)

    @Test
    fun secondLevelHeadingsAreDistinctAndInCatalogOrder() {
        assertEquals(
            listOf(CityPickerRow.Heading("West Midlands"), CityPickerRow.CityRow(cheshire), CityPickerRow.CityRow(london)),
            ukCities.secondLevelRows("", "uk", "England"),
        )
    }

    @Test
    fun aRegionWithNoSplitCountyHasNoSubregionHeadings() {
        val flatOnly = listOf(cheshire, london)
        assertEquals(
            listOf(CityPickerRow.CityRow(cheshire), CityPickerRow.CityRow(london)),
            flatOnly.secondLevelRows("", "uk", "England"),
        )
    }

    @Test
    fun secondLevelRowSearchFoldsLikeCityNamesDo() {
        assertEquals(listOf(CityPickerRow.Heading("West Midlands")), ukCities.secondLevelRows("west", "uk", "England"))
        assertEquals(emptyList<CityPickerRow>(), ukCities.secondLevelRows("zzz", "uk", "England"))
    }

    /**
     * The "West Midlands" heading sits INTERLEAVED at its own position in the
     * catalog's order, between the direct cities on either side of it — not
     * stranded ahead of every direct row. This is the exact shape the
     * reported "West Midlands out of order" bug had.
     */
    @Test
    fun aSubregionHeadingIsInterleavedWithDirectCitiesNotStrandedAheadOfThem() {
        val ordered = listOf(cheshire, birmingham, dudley, london)
        assertEquals(
            listOf(CityPickerRow.CityRow(cheshire), CityPickerRow.Heading("West Midlands"), CityPickerRow.CityRow(london)),
            ordered.secondLevelRows("", "uk", "England"),
        )
    }

    @Test
    fun directRowsExcludeThoseWithASubregion() {
        val directCities = ukCities.secondLevelRows("", "uk", "England")
            .filterIsInstance<CityPickerRow.CityRow>()
            .map { it.city.slug }
        assertEquals(listOf("cheshire", "london"), directCities)
    }
}
