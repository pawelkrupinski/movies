package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Test
import pl.kinowo.TestData.cinema
import pl.kinowo.TestData.day
import pl.kinowo.TestData.film
import pl.kinowo.TestData.slot
import pl.kinowo.filter.SortOption
import pl.kinowo.filter.sortedFor
import pl.kinowo.model.Ratings

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
}
