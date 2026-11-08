package pl.kinowo.filter

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Showtime

/**
 * The repertoire's filter universes and the "any filter on?" rule, as pure data —
 * no ViewModel. Parallel to iOS `ListingFiltersTests`.
 */
class FilterFacetsTest {

    private fun playing(vararg cinemas: String) =
        listOf(DayShowings("2026-11-08", "", cinemas.map { CinemaShowings(it, null, listOf(Showtime("18:00"))) }))

    @Test
    fun nameCountsOrderByFrequencyThenCaseInsensitiveName() {
        val films = listOf(
            Film(title = "A", countries = listOf("Polska", "francja")),
            Film(title = "B", countries = listOf("Francja2", "Polska")),
            Film(title = "C", countries = listOf("Austria")),
        )
        assertEquals(
            listOf(NameCount("Polska", 2), NameCount("Austria", 1), NameCount("francja", 1), NameCount("Francja2", 1)),
            FilterFacets.allCountries(films),
        )
    }

    @Test
    fun eachFacetReadsItsOwnAxis() {
        val films = listOf(Film(title = "A", genres = listOf("Dramat"), directors = listOf("Wajda"), cast = listOf("Cybulski")))
        assertEquals(listOf("Dramat"), FilterFacets.allGenres(films).map { it.name })
        assertEquals(listOf("Wajda"), FilterFacets.allDirectors(films).map { it.name })
        assertEquals(listOf("Cybulski"), FilterFacets.allCast(films).map { it.name })
    }

    @Test
    fun allCinemasAreDistinctAcrossFilmsAndDays() {
        val films = listOf(Film(title = "A", showings = playing("Muza", "Apollo")), Film(title = "B", showings = playing("Muza")))
        assertEquals(setOf("Muza", "Apollo"), FilterFacets.allCinemas(films).toSet())
        assertEquals(2, FilterFacets.allCinemas(films).size)
    }

    /** A cinema unticked in ANOTHER city lingers in the global set; it must not light the icon here. */
    @Test
    fun anotherCitysDisabledCinemaDoesNotMakeTheFiltersActive() {
        val films = listOf(Film(title = "A", showings = playing("Muza")))
        assertFalse(FilterFacets.filtersActive(films, FilterAxes(), listOf("Muza"), setOf("Elsewhere"), emptySet()))
        assertTrue(FilterFacets.filtersActive(films, FilterAxes(), listOf("Muza"), setOf("Muza"), emptySet()))
        assertTrue(FilterFacets.filtersActive(films, FilterAxes(excludedCast = setOf("X")), listOf("Muza"), emptySet(), emptySet()))
        assertTrue(FilterFacets.filtersActive(films, FilterAxes(), listOf("Muza"), emptySet(), setOf("Hidden")))
    }

    /**
     * The Filtry icon's rule, case for case the same as iOS `ListingFiltersTests` (`ListingFilters.isActive`), so the
     * two apps cannot drift: every narrowing counts (a satisfiable format axis, a name exclusion, a cinema unticked in
     * this city, a hidden film); sort, search, another city's cinema and an unsatisfiable IMAX pick do not.
     */
    @Test
    fun theFiltersActiveRuleMatchesIos() {
        val films = listOf(Film(title = "A", showings = playing("Muza")))
        val cityCinemas = FilterFacets.allCinemas(films)
        fun active(axes: FilterAxes = FilterAxes(), disabled: Set<String> = emptySet(), hidden: Set<String> = emptySet()) =
            FilterFacets.filtersActive(films, axes, cityCinemas, disabled, hidden)

        assertFalse("nothing set", active())
        assertFalse("sort alone", active(FilterAxes(sort = SortOption.RATING)))
        assertFalse("search alone", active(FilterAxes(query = "A")))
        assertFalse("unsatisfiable IMAX", active(FilterAxes(format = FormatFilter(imax = true))))
        assertFalse("another city's cinema", active(disabled = setOf("Elsewhere")))
        for (format in listOf(FormatFilter(dimension = "3D"), FormatFilter(language = "NAP"), FormatFilter(fromHour = 18))) {
            assertTrue("$format", active(FilterAxes(format = format)))
        }
        assertTrue("country", active(FilterAxes(excludedCountries = setOf("X"))))
        assertTrue("genre", active(FilterAxes(excludedGenres = setOf("X"))))
        assertTrue("director", active(FilterAxes(excludedDirectors = setOf("X"))))
        assertTrue("cast", active(FilterAxes(excludedCast = setOf("X"))))
        assertTrue("this city's cinema", active(disabled = setOf("Muza")))
        assertTrue("hidden film", active(hidden = setOf("Hidden")))
    }
}
