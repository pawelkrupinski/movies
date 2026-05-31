package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.TestData.cinema
import pl.kinowo.TestData.day
import pl.kinowo.TestData.film
import pl.kinowo.TestData.slot
import pl.kinowo.filter.groupedByCinema
import pl.kinowo.model.Film

class GroupedByCinemaTest {

    @Test
    fun emptyInputReturnsEmptyList() {
        assertTrue(emptyList<Film>().groupedByCinema().isEmpty())
    }

    @Test
    fun sectionsSortedAlphabeticallyByCinemaName() {
        val films = listOf(
            film("F1", listOf(day("2026-05-22", listOf(
                cinema("Muza", listOf(slot("10:00"))),
                cinema("Apollo", listOf(slot("11:00"))),
                cinema("Helonki", listOf(slot("12:00"))),
            )))),
        )
        val sections = films.groupedByCinema()
        assertEquals(listOf("Apollo", "Helonki", "Muza"), sections.map { it.cinema })
    }

    @Test
    fun filmAtTwoCinemasAppearsInBothSections() {
        val films = listOf(
            film("Dune", listOf(day("2026-05-22", listOf(
                cinema("Apollo", listOf(slot("18:00"))),
                cinema("Helonki", listOf(slot("20:00"))),
            )))),
        )
        val sections = films.groupedByCinema()
        assertEquals(2, sections.size)
        assertEquals(listOf("Apollo", "Helonki"), sections.map { it.cinema })
        assertEquals(listOf("Dune"), sections[0].films.map { it.title })
        assertEquals(listOf("Dune"), sections[1].films.map { it.title })
    }

    @Test
    fun eachOutputFilmCarriesOnlyThatCinemasSlots() {
        val films = listOf(
            film("Dune", listOf(day("2026-05-22", listOf(
                cinema("Apollo", listOf(slot("18:00"), slot("21:00"))),
                cinema("Helonki", listOf(slot("20:00"))),
            )))),
        )
        val sections = films.groupedByCinema()
        val apollo = sections.first { it.cinema == "Apollo" }
        val helonki = sections.first { it.cinema == "Helonki" }

        assertEquals(1, apollo.films[0].showings.size)
        assertEquals(listOf("Apollo"), apollo.films[0].showings[0].cinemas.map { it.cinema })
        assertEquals(listOf("18:00", "21:00"), apollo.films[0].showings[0].cinemas[0].showtimes.map { it.time })

        assertEquals(1, helonki.films[0].showings.size)
        assertEquals(listOf("Helonki"), helonki.films[0].showings[0].cinemas.map { it.cinema })
        assertEquals(listOf("20:00"), helonki.films[0].showings[0].cinemas[0].showtimes.map { it.time })
    }

    @Test
    fun filmAcrossMultipleDaysGroupsByCinema() {
        val films = listOf(
            film("Belle", listOf(
                day("2026-05-22", listOf(cinema("Apollo", listOf(slot("18:00"))))),
                day("2026-05-23", listOf(
                    cinema("Apollo", listOf(slot("19:00"))),
                    cinema("Helonki", listOf(slot("20:00"))),
                )),
            )),
        )
        val sections = films.groupedByCinema()
        assertEquals(listOf("Apollo", "Helonki"), sections.map { it.cinema })

        val apollo = sections[0].films[0]
        assertEquals(listOf("2026-05-22", "2026-05-23"), apollo.showings.map { it.date })

        val helonki = sections[1].films[0]
        assertEquals(listOf("2026-05-23"), helonki.showings.map { it.date })
    }

    @Test
    fun filmsInSectionPreserveInputOrder() {
        val films = listOf(
            film("Zebra", listOf(day("2026-05-22", listOf(cinema("Apollo", listOf(slot("18:00"))))))),
            film("Alpha", listOf(day("2026-05-22", listOf(cinema("Apollo", listOf(slot("19:00"))))))),
            film("Middle", listOf(day("2026-05-22", listOf(cinema("Apollo", listOf(slot("20:00"))))))),
        )
        val sections = films.groupedByCinema()
        assertEquals(1, sections.size)
        assertEquals(listOf("Zebra", "Alpha", "Middle"), sections[0].films.map { it.title })
    }
}
