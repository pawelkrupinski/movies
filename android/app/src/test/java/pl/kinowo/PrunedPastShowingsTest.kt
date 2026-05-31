package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Test
import pl.kinowo.TestData.warsawInstant
import pl.kinowo.filter.prunedPastShowings
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Ratings
import pl.kinowo.model.Showtime
import java.time.Instant

class PrunedPastShowingsTest {

    private fun slot(time: String): Showtime =
        Showtime(time = time, format = "2D", room = null, bookingURL = null)

    private fun cinema(name: String, times: List<Showtime>): CinemaShowings =
        CinemaShowings(cinema = name, cinemaURL = null, showtimes = times)

    private fun day(date: String, cinemas: List<CinemaShowings>): DayShowings =
        DayShowings(date = date, label = date, cinemas = cinemas)

    private fun film(title: String, days: List<DayShowings>): Film =
        Film(
            title = title, posterURL = null, fallbackPosterURLs = emptyList(),
            runtimeMinutes = 90, ratings = Ratings.EMPTY, countries = emptyList(),
            directors = emptyList(), cast = emptyList(), showings = days,
        )

    private fun pinnedNow(): Instant = warsawInstant(2026, 5, 22, 18, 0)

    @Test
    fun dropsSlotPast30MinuteGraceKeepsSlotInside() {
        val now = pinnedNow()
        val films = listOf(film("F", listOf(day("2026-05-22", listOf(
            cinema("Helonki", listOf(slot("17:29"), slot("17:35"))),
        )))))
        val pruned = films.prunedPastShowings(now)
        val kept = pruned[0].showings[0].cinemas[0].showtimes.map { it.time }
        assertEquals(listOf("17:35"), kept)
    }

    @Test
    fun dropsSlotExactlyAtNowMinus30() {
        // Strict isAfter(now - 30min) — a slot at exactly that moment is dropped.
        val now = pinnedNow()
        val films = listOf(film("F", listOf(day("2026-05-22", listOf(
            cinema("Helonki", listOf(slot("17:30"), slot("17:31"))),
        )))))
        val pruned = films.prunedPastShowings(now)
        val kept = pruned[0].showings[0].cinemas[0].showtimes.map { it.time }
        assertEquals(listOf("17:31"), kept)
    }

    @Test
    fun cinemasInsideDayReorderedByEarliestRemaining() {
        val now = pinnedNow()
        val films = listOf(film("F", listOf(day("2026-05-22", listOf(
            cinema("Apollo", listOf(slot("15:00"), slot("18:30"))),
            cinema("Helonki", listOf(slot("16:00"), slot("17:35"))),
        )))))
        val pruned = films.prunedPastShowings(now)
        val order = pruned[0].showings[0].cinemas.map { it.cinema }
        assertEquals(listOf("Helonki", "Apollo"), order)
    }

    @Test
    fun dayWithNoRemainingCinemasRemoved() {
        val now = pinnedNow()
        val films = listOf(film("F", listOf(
            day("2026-05-22", listOf(cinema("Muza", listOf(slot("10:00"), slot("12:00"))))),
            day("2026-05-23", listOf(cinema("Apollo", listOf(slot("10:00"))))),
        )))
        val pruned = films.prunedPastShowings(now)
        assertEquals(listOf("2026-05-23"), pruned[0].showings.map { it.date })
    }

    @Test
    fun filmWithNoRemainingDaysRemovedEntirely() {
        val now = pinnedNow()
        val films = listOf(
            film("AllPast", listOf(day("2026-05-22", listOf(
                cinema("Muza", listOf(slot("10:00"), slot("13:00"))),
            )))),
            film("Live", listOf(day("2026-05-22", listOf(
                cinema("Apollo", listOf(slot("19:00"))),
            )))),
        )
        val pruned = films.prunedPastShowings(now)
        assertEquals(listOf("Live"), pruned.map { it.title })
    }

    @Test
    fun tomorrowSlotsUntouched() {
        val now = pinnedNow()
        val films = listOf(film("F", listOf(
            day("2026-05-23", listOf(cinema("Apollo", listOf(
                slot("10:00"), slot("12:30"), slot("16:00"),
            )))),
        )))
        val pruned = films.prunedPastShowings(now)
        assertEquals(
            listOf("10:00", "12:30", "16:00"),
            pruned[0].showings[0].cinemas[0].showtimes.map { it.time },
        )
    }

    @Test
    fun idempotent() {
        val now = pinnedNow()
        val films = listOf(film("F", listOf(day("2026-05-22", listOf(
            cinema("Apollo", listOf(slot("15:00"), slot("18:30"))),
            cinema("Helonki", listOf(slot("17:35"), slot("19:00"))),
        )))))
        val once = films.prunedPastShowings(now)
        val twice = once.prunedPastShowings(now)
        assertEquals(once, twice)
    }

    @Test
    fun composedScenarioMirrorsWebSemantics() {
        val now = pinnedNow()
        val today = "2026-05-22"
        val tomorrow = "2026-05-23"

        val helonki = cinema("Helonki", listOf(
            slot("16:00"), slot("17:30"), slot("17:35"), slot("19:00"),
        ))
        val apollo = cinema("Apollo", listOf(slot("15:00"), slot("18:30")))
        val muza = cinema("Muza", listOf(slot("10:00"), slot("12:30")))

        val liveFilm = film("Live", listOf(
            day(today, listOf(apollo, helonki, muza)),
            day(tomorrow, listOf(cinema("Apollo", listOf(slot("10:00"))))),
        ))
        val allPast = film("All-Past", listOf(
            day(today, listOf(cinema("Muza", listOf(slot("10:00"), slot("13:00"))))),
        ))

        val pruned = listOf(liveFilm, allPast).prunedPastShowings(now)
        assertEquals(listOf("Live"), pruned.map { it.title })

        val todayDay = pruned[0].showings.first { it.date == today }
        assertEquals(listOf("Helonki", "Apollo"), todayDay.cinemas.map { it.cinema })
        val helonkiKept = todayDay.cinemas.first { it.cinema == "Helonki" }
        assertEquals(listOf("17:35", "19:00"), helonkiKept.showtimes.map { it.time })
        val apolloKept = todayDay.cinemas.first { it.cinema == "Apollo" }
        assertEquals(listOf("18:30"), apolloKept.showtimes.map { it.time })

        val tomorrowDay = pruned[0].showings.first { it.date == tomorrow }
        assertEquals(1, tomorrowDay.cinemas.size)
        assertEquals(listOf("10:00"), tomorrowDay.cinemas[0].showtimes.map { it.time })
    }
}
