package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Test
import pl.kinowo.TestData.cinema
import pl.kinowo.TestData.day
import pl.kinowo.TestData.film
import pl.kinowo.TestData.slot
import pl.kinowo.filter.DateFilter
import pl.kinowo.filter.FormatFilter
import pl.kinowo.filter.SortOption
import pl.kinowo.filter.filteredFor
import pl.kinowo.filter.sortedFor
import pl.kinowo.model.Ratings
import java.time.Instant
import java.time.ZoneId

/**
 * The "Sortuj" axis mirroring the web's `compareCards` (public/js/shared.js):
 * earliest-showing ascending (default) or weighted-rating descending, both
 * stable for full ties.
 */
class SortedForTest {

    private fun rated(imdb: Double? = null, filmweb: Double? = null, metascore: Int? = null, rt: Int? = null) =
        Ratings(imdb = imdb, filmweb = filmweb, metascore = metascore, rottenTomatoes = rt)

    @Test
    fun weightedRatingAveragesPresentScoresOnATenScale() {
        assertEquals(0.0, Ratings.EMPTY.weighted, 0.0001)
        assertEquals(8.0, rated(imdb = 8.0).weighted, 0.0001)
        assertEquals(5.0, rated(rt = 50).weighted, 0.0001)
        // imdb 8.0 and metascore 90 → (8.0 + 9.0) / 2.
        assertEquals(8.5, rated(imdb = 8.0, metascore = 90).weighted, 0.0001)
        // imdb 6, filmweb 8, metascore 40→4, rt 100→10 → 28/4.
        assertEquals(7.0, rated(imdb = 6.0, filmweb = 8.0, metascore = 40, rt = 100).weighted, 0.0001)
    }

    @Test
    fun earliestSortsByNearestShowingAscending() {
        val films = listOf(
            film("A", listOf(day("2026-05-22", listOf(cinema("X", listOf(slot("18:00"))))))),
            film("B", listOf(day("2026-05-22", listOf(cinema("X", listOf(slot("09:00"))))))),
            film("C", listOf(day("2026-05-21", listOf(cinema("X", listOf(slot("23:00"))))))),
        )
        assertEquals(listOf("C", "B", "A"), films.sortedFor(SortOption.EARLIEST).map { it.title })
    }

    @Test
    fun earliestUsesTheFilmsMinimumSlotAcrossDaysAndCinemas() {
        val films = listOf(
            film("Late", listOf(day("2026-05-22", listOf(cinema("X", listOf(slot("20:00"))))))),
            film("EarlyBuried", listOf(day("2026-05-22", listOf(
                cinema("X", listOf(slot("21:00"))),
                cinema("Y", listOf(slot("08:30"), slot("22:00"))),
            )))),
        )
        assertEquals(listOf("EarlyBuried", "Late"), films.sortedFor(SortOption.EARLIEST).map { it.title })
    }

    @Test
    fun earliestIsStableForTies() {
        val films = listOf(
            film("First", listOf(day("2026-05-22", listOf(cinema("X", listOf(slot("10:00"))))))),
            film("Second", listOf(day("2026-05-22", listOf(cinema("X", listOf(slot("10:00"))))))),
        )
        assertEquals(listOf("First", "Second"), films.sortedFor(SortOption.EARLIEST).map { it.title })
    }

    @Test
    fun ratingSortsByWeightedRatingDescending() {
        val films = listOf(
            // C plays earliest but is unrated → sinks below the rated films.
            film("C", listOf(day("2026-05-21", listOf(cinema("X", listOf(slot("23:00")))))), ratings = Ratings.EMPTY),
            film("A", listOf(day("2026-05-22", listOf(cinema("X", listOf(slot("18:00")))))), ratings = rated(imdb = 9.0)),
            film("B", listOf(day("2026-05-22", listOf(cinema("X", listOf(slot("09:00")))))), ratings = rated(imdb = 5.0)),
        )
        assertEquals(listOf("A", "B", "C"), films.sortedFor(SortOption.RATING).map { it.title })
    }

    @Test
    fun ratingTieBreaksOnEarliestShowing() {
        val films = listOf(
            film("Later", listOf(day("2026-05-22", listOf(cinema("X", listOf(slot("20:00")))))), ratings = rated(imdb = 7.0)),
            film("Sooner", listOf(day("2026-05-22", listOf(cinema("X", listOf(slot("10:00")))))), ratings = rated(imdb = 7.0)),
        )
        assertEquals(listOf("Sooner", "Later"), films.sortedFor(SortOption.RATING).map { it.title })
    }

    // ── The composed pipeline, not just the comparator ─────────────────────
    //
    // Every test above sorts a hand-built list directly. Production never does:
    // `KinowoViewModel.filmsFor` runs `filteredFor(day, …).sortedFor(sort)`, and
    // it is the COMPOSITION that has to hold — the sort key must be recomputed
    // from the showings that survived filtering, not the ones the server ranked
    // by. iOS shipped that composition broken (its `.earliest` was a no-op) and
    // no comparator-level test could see it. These pin the composition.

    @Test
    fun theDayPageIsRankedByThatDaysShowings() {
        // The server ranks the payload by each film's earliest showtime across
        // the WHOLE schedule, so "Opener" (today 10:00) arrives first. On the
        // tomorrow page it plays at 22:00 and "Sleeper" at 09:00.
        val payloadOrder = listOf(
            film("Opener", listOf(
                day("2026-05-22", listOf(cinema("X", listOf(slot("10:00"))))),
                day("2026-05-23", listOf(cinema("X", listOf(slot("22:00"))))),
            )),
            film("Sleeper", listOf(day("2026-05-23", listOf(cinema("X", listOf(slot("09:00"))))))),
        )
        assertEquals(listOf("Opener", "Sleeper"), payloadOrder.sortedFor(SortOption.EARLIEST).map { it.title })

        val tomorrow = payloadOrder
            .filteredFor(
                date = DateFilter.Specific("2026-05-23"), format = FormatFilter(),
                query = "", hidden = emptySet(),
                now = Instant.parse("2026-05-22T08:00:00Z"), zone = ZoneId.of("Europe/Warsaw"),
            )
            .sortedFor(SortOption.EARLIEST)
        assertEquals(listOf("Sleeper", "Opener"), tomorrow.map { it.title })
    }

    @Test
    fun theFromHourFilterRanksByTheSlotsItLeavesVisible() {
        val films = listOf(
            film("Matinee", listOf(day("2026-05-22", listOf(cinema("X", listOf(slot("09:00"), slot("23:00"))))))),
            film("Primetime", listOf(day("2026-05-22", listOf(cinema("X", listOf(slot("20:00"))))))),
        )
        assertEquals(listOf("Matinee", "Primetime"), films.sortedFor(SortOption.EARLIEST).map { it.title })

        val fromEight = films
            .filteredFor(
                date = DateFilter.Kind.ANYTIME, format = FormatFilter(fromHour = 20),
                query = "", hidden = emptySet(),
                now = Instant.parse("2026-05-22T06:00:00Z"), zone = ZoneId.of("Europe/Warsaw"),
            )
            .sortedFor(SortOption.EARLIEST)
        assertEquals(listOf("Primetime", "Matinee"), fromEight.map { it.title })
    }

    @Test
    fun aDisabledCinemaRanksByWhatTheRemainingOnesShow() {
        val films = listOf(
            film("EarlyAtDropped", listOf(day("2026-05-22", listOf(
                cinema("Dropped", listOf(slot("08:00"))),
                cinema("Kept", listOf(slot("21:00"))),
            )))),
            film("Steady", listOf(day("2026-05-22", listOf(cinema("Kept", listOf(slot("12:00"))))))),
        )
        assertEquals(listOf("EarlyAtDropped", "Steady"), films.sortedFor(SortOption.EARLIEST).map { it.title })

        val withoutDropped = films
            .filteredFor(
                date = DateFilter.Kind.ANYTIME, format = FormatFilter(),
                query = "", hidden = emptySet(), disabledCinemas = setOf("Dropped"),
                now = Instant.parse("2026-05-22T06:00:00Z"), zone = ZoneId.of("Europe/Warsaw"),
            )
            .sortedFor(SortOption.EARLIEST)
        assertEquals(listOf("Steady", "EarlyAtDropped"), withoutDropped.map { it.title })
    }
}
