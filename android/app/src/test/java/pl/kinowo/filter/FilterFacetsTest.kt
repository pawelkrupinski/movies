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
}
