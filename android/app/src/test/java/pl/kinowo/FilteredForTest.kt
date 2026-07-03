package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.TestData.cinema
import pl.kinowo.TestData.day
import pl.kinowo.TestData.film
import pl.kinowo.TestData.slot
import pl.kinowo.TestData.warsawInstant
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.FormatFilter
import pl.kinowo.filter.filteredFor
import pl.kinowo.model.Film
import java.time.Instant

class FilteredForTest {

    private val now: Instant = Instant.now()
    private val today: String = DateFilter.iso(now)
    private val tomorrow: String = DateFilter.iso(now.plusSeconds(86_400))

    private fun fixture(): List<Film> {
        val mandalorian = film("Mandalorian and Grogu", listOf(
            day(today, listOf(
                cinema("Helonki", listOf(slot("17:00"), slot("20:00", "3D NAP"))),
                cinema("Apollo", listOf(slot("19:30"))),
            )),
            day(tomorrow, listOf(
                cinema("Muza", listOf(slot("16:00"))),
            )),
        ))
        val title2 = film("Title2", listOf(
            day(today, listOf(
                cinema("Apollo", listOf(slot("18:00"))),
            )),
        ))
        val title3 = film("Title3", listOf(
            day(tomorrow, listOf(
                cinema("Helonki", listOf(slot("21:00"))),
            )),
        ))
        return listOf(mandalorian, title2, title3)
    }

    @Test
    fun dateTodayFiltersToTodayOnly() {
        val result = fixture().filteredFor(
            date = DateFilter.Today, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, now = now,
        )
        assertEquals(listOf("Mandalorian and Grogu", "Title2"), result.map { it.title }.sorted())
        for (f in result) {
            assertEquals(listOf(today), f.showings.map { it.date })
        }
    }

    @Test
    fun dateTomorrowKeepsTomorrowShowings() {
        // Bug 2 probe: "Jutro" wrongly showed "Brak repertuaru." while tomorrow
        // genuinely had repertoire. Run a fixture with tomorrow showings through
        // the real filter and assert it is NON-EMPTY and dated to tomorrow only.
        val result = fixture().filteredFor(
            date = DateFilter.Tomorrow, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, now = now,
        )
        assertEquals(listOf("Mandalorian and Grogu", "Title3"), result.map { it.title }.sorted())
        for (f in result) {
            assertEquals(listOf(tomorrow), f.showings.map { it.date })
        }
    }

    @Test
    fun tomorrowFilterUsesProvidedNowAtMidnightBoundary() {
        // A pinned clock at 23:50 today must still match tomorrow's date (date
        // matching is in Warsaw wall-clock, not UTC — guards a midnight/timezone
        // boundary regression).
        val fixedTomorrow = "2020-06-16"
        val lateNightNow = warsawInstant(2020, 6, 15, 23, 50)

        val films = listOf(
            film("A", listOf(day("2020-06-15", listOf(cinema("X", listOf(slot("18:00"))))))),
            film("B", listOf(day(fixedTomorrow, listOf(cinema("X", listOf(slot("19:00"))))))),
        )

        val filtered = films.filteredFor(
            date = DateFilter.Tomorrow, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, now = lateNightNow,
        )
        assertEquals(listOf("B"), filtered.map { it.title })
        assertEquals(listOf(fixedTomorrow), filtered[0].showings.map { it.date })
    }

    @Test
    fun queryMatchesCaseInsensitiveSubstring() {
        val result = fixture().filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "Mand",
            hidden = emptySet(), selectedCinema = null, now = now,
        )
        assertEquals(listOf("Mandalorian and Grogu"), result.map { it.title })

        val lowered = fixture().filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "mand",
            hidden = emptySet(), selectedCinema = null, now = now,
        )
        assertEquals(listOf("Mandalorian and Grogu"), lowered.map { it.title })
    }

    @Test
    fun hiddenDropsFilmEntirely() {
        val result = fixture().filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = setOf("Title2"), selectedCinema = null, now = now,
        )
        assertFalse(result.any { it.title == "Title2" })
        assertEquals(2, result.size)
    }

    @Test
    fun selectedCinemaNullKeepsEveryCinemasShowings() {
        val result = fixture().filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, now = now,
        )
        // All three films survive and Mandalorian keeps BOTH of today's cinemas.
        assertEquals(listOf("Mandalorian and Grogu", "Title2", "Title3"), result.map { it.title }.sorted())
        val mando = result.first { it.title == "Mandalorian and Grogu" }
        val todayDay = mando.showings.first { it.date == today }
        assertEquals(listOf("Helonki", "Apollo"), todayDay.cinemas.map { it.cinema })
    }

    @Test
    fun selectedCinemaKeepsOnlyThatCinemaAndDropsFilmsWithNone() {
        val result = fixture().filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = "Apollo", now = now,
        )
        // Only Apollo screenings remain: Mandalorian (today) + Title2 (today).
        // Title3 (Helonki only) drops; Mandalorian's Helonki + Muza groups drop.
        assertEquals(listOf("Mandalorian and Grogu", "Title2"), result.map { it.title }.sorted())
        val mando = result.first { it.title == "Mandalorian and Grogu" }
        assertEquals(listOf(today), mando.showings.map { it.date })
        assertEquals(listOf("Apollo"), mando.showings.first().cinemas.map { it.cinema })
    }

    @Test
    fun selectedCinemaAbsentFromListingDropsEveryFilm() {
        // The cross-city guard is applied UPSTREAM (the pill bar passes null when
        // the pick isn't in the current city), so filteredFor itself honours the
        // name literally: a cinema that appears nowhere keeps nothing. This pins
        // that behaviour so the upstream guard is load-bearing, not incidental.
        val result = fixture().filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = "Nonexistent Cinema", now = now,
        )
        assertTrue(result.isEmpty())
    }

    @Test
    fun formatNarrowsShowtimesButKeepsFilmIfAnySlotRemains() {
        val f = FormatFilter(dimension = "3D")
        val result = fixture().filteredFor(
            date = DateFilter.Anytime, format = f, query = "",
            hidden = emptySet(), selectedCinema = null, now = now,
        )
        assertEquals(listOf("Mandalorian and Grogu"), result.map { it.title })
        val mando = result[0]
        val todayDay = mando.showings.first { it.date == today }
        assertEquals(1, todayDay.cinemas.size)
        assertEquals("Helonki", todayDay.cinemas[0].cinema)
        assertEquals(listOf("20:00"), todayDay.cinemas[0].showtimes.map { it.time })
    }

    @Test
    fun emptyQueryTrimsWhitespace() {
        val result = fixture().filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "   ",
            hidden = emptySet(), selectedCinema = null, now = now,
        )
        assertEquals(3, result.size)
    }

    @Test
    fun combinedFiltersIntersect() {
        // Date=Today ∧ query="Mand" ∧ hidden={Title2} ∧ selectedCinema="Helonki":
        // only Mandalorian survives, narrowed to its Helonki group today.
        val result = fixture().filteredFor(
            date = DateFilter.Today, format = FormatFilter.EMPTY, query = "Mand",
            hidden = setOf("Title2"), selectedCinema = "Helonki", now = now,
        )
        assertEquals(listOf("Mandalorian and Grogu"), result.map { it.title })
        val todayDay = result[0].showings[0]
        assertEquals(listOf("Helonki"), todayDay.cinemas.map { it.cinema })
    }

    @Test
    fun todayFilterUsesProvidedNowNotSystemClock() {
        val fixedToday = "2020-06-15"
        val fixedTomorrow = "2020-06-16"
        val pinnedNow = warsawInstant(2020, 6, 15, 12, 0)

        val films = listOf(
            film("A", listOf(day(fixedToday, listOf(cinema("X", listOf(slot("18:00"))))))),
            film("B", listOf(day(fixedTomorrow, listOf(cinema("X", listOf(slot("19:00"))))))),
        )

        val filtered = films.filteredFor(
            date = DateFilter.Today, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, now = pinnedNow,
        )
        assertEquals(listOf("A"), filtered.map { it.title })
        assertEquals(listOf(fixedToday), filtered[0].showings.map { it.date })
    }

    // Country filter (excluded semantics) — NO empty-guard.

    @Test
    fun excludedCountryHidesFilmsOnlyFromThatCountry() {
        val films = listOf(
            film("Polish Film", listOf(day(today, listOf(cinema("A", listOf(slot("18:00")))))), countries = listOf("Polska")),
            film("US Film", listOf(day(today, listOf(cinema("A", listOf(slot("19:00")))))), countries = listOf("USA")),
            film("Co-prod", listOf(day(today, listOf(cinema("A", listOf(slot("20:00")))))), countries = listOf("Polska", "Francja")),
        )
        val filtered = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedCountries = setOf("Polska"), now = now,
        )
        assertEquals(listOf("Co-prod", "US Film"), filtered.map { it.title }.sorted())
    }

    @Test
    fun excludedCountryHidesCoProductionOnlyWhenAllExcluded() {
        val films = listOf(
            film("Co-prod", listOf(day(today, listOf(cinema("A", listOf(slot("18:00")))))), countries = listOf("Polska", "Francja")),
        )
        val still = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedCountries = setOf("Polska"), now = now,
        )
        assertEquals("co-prod stays when only one country is excluded", 1, still.size)

        val gone = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedCountries = setOf("Polska", "Francja"), now = now,
        )
        assertEquals("co-prod drops when all its countries are excluded", 0, gone.size)
    }

    @Test
    fun emptyCountriesFilmHiddenWhenAnyCountryExcluded() {
        // Countries has NO empty-guard: a film with empty countries is a subset
        // of any non-empty exclusion set, so it drops.
        val films = listOf(
            film("NoCountry", listOf(day(today, listOf(cinema("A", listOf(slot("18:00")))))), countries = emptyList()),
        )
        val filtered = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedCountries = setOf("Polska"), now = now,
        )
        assertEquals(0, filtered.size)
    }

    @Test
    fun emptyExcludedCountriesShowsAll() {
        val films = listOf(
            film("A", listOf(day(today, listOf(cinema("A", listOf(slot("18:00")))))), countries = listOf("Polska")),
            film("B", listOf(day(today, listOf(cinema("A", listOf(slot("19:00")))))), countries = listOf("USA")),
        )
        val filtered = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedCountries = emptySet(), now = now,
        )
        assertEquals(2, filtered.size)
    }

    // Genre filter — HAS empty-guard (mirrors director/cast, not country).

    @Test
    fun excludedGenreHidesFilmsOnlyFromThatGenre() {
        val films = listOf(
            film("Comedy", listOf(day(today, listOf(cinema("A", listOf(slot("18:00")))))), genres = listOf("Komedia")),
            film("Drama", listOf(day(today, listOf(cinema("A", listOf(slot("19:00")))))), genres = listOf("Dramat")),
            film("Mix", listOf(day(today, listOf(cinema("A", listOf(slot("20:00")))))), genres = listOf("Komedia", "Dramat")),
        )
        val filtered = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedGenres = setOf("Komedia"), now = now,
        )
        assertEquals(listOf("Drama", "Mix"), filtered.map { it.title }.sorted())
    }

    @Test
    fun excludedGenreDropsMultiGenreFilmOnlyWhenAllExcluded() {
        val films = listOf(
            film("Mix", listOf(day(today, listOf(cinema("A", listOf(slot("18:00")))))), genres = listOf("Komedia", "Dramat")),
        )
        assertEquals(1, films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedGenres = setOf("Komedia"), now = now,
        ).size)
        assertEquals(0, films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedGenres = setOf("Komedia", "Dramat"), now = now,
        ).size)
    }

    @Test
    fun filmWithNoGenresSurvivesGenreExclusion() {
        // Empty-guard: a film with no genres must not drop when a genre is excluded.
        val films = listOf(
            film("NoGenre", listOf(day(today, listOf(cinema("A", listOf(slot("18:00")))))), genres = emptyList()),
        )
        val filtered = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedGenres = setOf("Komedia"), now = now,
        )
        assertEquals(1, filtered.size)
    }

    // Director filter — HAS empty-guard.

    @Test
    fun excludedDirectorHidesFilm() {
        val films = listOf(
            film("A", listOf(day(today, listOf(cinema("X", listOf(slot("18:00")))))), directors = listOf("Spielberg")),
            film("B", listOf(day(today, listOf(cinema("X", listOf(slot("19:00")))))), directors = listOf("Nolan")),
            film("C", listOf(day(today, listOf(cinema("X", listOf(slot("20:00")))))), directors = listOf("Spielberg", "Nolan")),
        )
        val filtered = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedDirectors = setOf("Spielberg"), now = now,
        )
        assertEquals(listOf("B", "C"), filtered.map { it.title }.sorted())
    }

    @Test
    fun excludedDirectorKeepsFilmWithNoDirector() {
        val films = listOf(
            film("Known", listOf(day(today, listOf(cinema("X", listOf(slot("18:00")))))), directors = listOf("Spielberg")),
            film("Unknown", listOf(day(today, listOf(cinema("X", listOf(slot("19:00")))))), directors = emptyList()),
        )
        val filtered = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedDirectors = setOf("Spielberg"), now = now,
        )
        assertEquals(listOf("Unknown"), filtered.map { it.title })
    }

    // Cast filter — HAS empty-guard.

    @Test
    fun excludedCastHidesFilm() {
        val films = listOf(
            film("A", listOf(day(today, listOf(cinema("X", listOf(slot("18:00")))))), cast = listOf("DiCaprio", "Pitt")),
            film("B", listOf(day(today, listOf(cinema("X", listOf(slot("19:00")))))), cast = listOf("Hanks")),
        )
        val filtered = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedCast = setOf("DiCaprio", "Pitt"), now = now,
        )
        assertEquals(listOf("B"), filtered.map { it.title })
    }

    @Test
    fun excludedCastKeepsFilmWhenOnlyPartialOverlap() {
        val films = listOf(
            film("A", listOf(day(today, listOf(cinema("X", listOf(slot("18:00")))))), cast = listOf("DiCaprio", "Pitt")),
        )
        val filtered = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedCast = setOf("DiCaprio"), now = now,
        )
        assertEquals("film stays when only part of its cast is excluded", 1, filtered.size)
    }

    @Test
    fun excludedCastKeepsFilmWithNoCast() {
        val films = listOf(
            film("Known", listOf(day(today, listOf(cinema("X", listOf(slot("18:00")))))), cast = listOf("Hanks")),
            film("Unknown", listOf(day(today, listOf(cinema("X", listOf(slot("19:00")))))), cast = emptyList()),
        )
        val filtered = films.filteredFor(
            date = DateFilter.Anytime, format = FormatFilter.EMPTY, query = "",
            hidden = emptySet(), selectedCinema = null, excludedCast = setOf("Hanks"), now = now,
        )
        assertEquals(listOf("Unknown"), filtered.map { it.title })
    }
}
