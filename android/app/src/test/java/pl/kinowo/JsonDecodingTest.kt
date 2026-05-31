package pl.kinowo

import kotlinx.serialization.json.Json
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails

/**
 * Replays recorded `/api/repertoire` + `/api/details` payloads through the
 * decoders, so a wire-shape drift (renamed field, changed nesting) fails here
 * rather than silently at runtime. Fixtures captured from the live endpoints.
 */
class JsonDecodingTest {
    private val json = Json { ignoreUnknownKeys = true }

    private fun fixture(name: String): String =
        javaClass.classLoader!!.getResourceAsStream("fixtures/$name")!!
            .bufferedReader().use { it.readText() }

    @Test
    fun `lean repertoire decodes into Films with full showings`() {
        val films = json.decodeFromString<List<Film>>(fixture("repertoire_sample.json"))
        assertTrue("expected a non-empty repertoire", films.isNotEmpty())
        val f = films.first()
        assertFalse("title should be non-blank", f.title.isBlank())
        // Year + genres ride on the lean listing so the card/detail can show
        // the same pills the web does.
        assertEquals(2026, f.releaseYear)
        assertEquals(listOf("Animacja", "Komedia", "Familijny"), f.genres)
        // The listing carries the full showings tree (date → cinema → showtime).
        val day = f.showings.firstOrNull()
        assertNotNull("expected at least one day of showings", day)
        val cg = day!!.cinemas.firstOrNull()
        assertNotNull("expected at least one cinema group", cg)
        assertTrue("expected at least one showtime", cg!!.showtimes.isNotEmpty())
        assertTrue("showtime time should look like HH:MM", cg.showtimes.first().time.contains(":"))
    }

    @Test
    fun `cinema links derive from the listing showings`() {
        val films = json.decodeFromString<List<Film>>(fixture("repertoire_sample.json"))
        // At least one fixture film should have a cinema with a URL → a link.
        val withLinks = films.firstOrNull { it.cinemaLinks.isNotEmpty() }
        assertNotNull("expected a film with derivable cinema links", withLinks)
        val link = withLinks!!.cinemaLinks.first()
        assertFalse(link.cinema.isBlank())
        assertFalse(link.url.isBlank())
        // Links are deduped by cinema and sorted alphabetically.
        val cinemas = withLinks.cinemaLinks.map { it.cinema }
        assertEquals("links should be deduped by cinema", cinemas.distinct(), cinemas)
        assertEquals("links should be alpha-sorted by cinema", cinemas.sorted(), cinemas)
    }

    @Test
    fun `details payload decodes synopsis and trailer embed urls`() {
        val details = json.decodeFromString<List<FilmDetails>>(fixture("details_sample.json"))
        assertTrue("expected a non-empty details payload", details.isNotEmpty())
        val withBoth = details.firstOrNull { !it.synopsis.isNullOrBlank() && it.trailerURLs.isNotEmpty() }
        assertNotNull("expected an entry with synopsis + trailers", withBoth)
        assertTrue(
            "trailer URLs should be ready-to-embed",
            withBoth!!.trailerURLs.all { it.contains("/embed/") || it.contains("player.vimeo") },
        )
    }
}
