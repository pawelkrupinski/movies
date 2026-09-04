package pl.kinowo.ui.common

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test

/**
 * `filmShareUrl` must mirror the server's `controllers.FilmHref`, so a link
 * shared from the app is byte-identical to one copied off the website. That is
 * now the city-scoped slug path `/<city>/movie/<slug>`; the legacy `?title=`
 * form stays for films the server sent no slug for, and must keep its exact
 * encoding — spaces as `%20` (never the form `+`), reserved characters and
 * Polish diacritics percent-encoded.
 */
class FilmShareUrlTest {

    /**
     * The origin is the COUNTRY's, not a constant. It was hardcoded to the
     * Polish host, so every share from the UK, Germany, the US or Spain went out
     * as a dead link — a Barcelona film as `kinowo.net/barcelona/movie/…`, which
     * 404s, because that city lives on `showtimes.cc/es`.
     */
    @Test
    fun `uses the browsing country's origin, not Poland's`() {
        assertEquals(
            "https://showtimes.cc/es/barcelona/movie/la-patrulla-canina-la-dino-pelicula",
            filmShareUrl("https://showtimes.cc/es", "barcelona", "La Patrulla Canina",
                         "la-patrulla-canina-la-dino-pelicula"),
        )
        assertEquals(
            "https://showtimes.cc/uk/london/movie/dune-part-two",
            filmShareUrl("https://showtimes.cc/uk", "london", "Dune: Part Two", "dune-part-two"),
        )
        assertEquals(
            "https://showtimes.cc/de/berlin/movie/dune-part-two",
            filmShareUrl("https://showtimes.cc/de", "berlin", "Dune: Part Two", "dune-part-two"),
        )
    }

    @Test
    fun `the query fallback is country-scoped too`() {
        assertEquals(
            "https://showtimes.cc/es/madrid/movie?title=Oppenheimer",
            filmShareUrl("https://showtimes.cc/es", "madrid", "Oppenheimer", null),
        )
    }

    /** No caller may reintroduce the constant: every share URL starts at the
     *  origin it was handed. */
    @Test
    fun `never emits the Polish host for another country`() {
        val url = filmShareUrl("https://showtimes.cc/us", "san-francisco", "Wicked", "wicked")
        assertFalse(url.contains("kinowo.net"))
    }

    @Test
    fun `prefers the server-supplied slug`() {
        assertEquals(
            "https://kinowo.net/wroclaw/movie/diuna-czesc-druga",
            filmShareUrl("https://kinowo.net", "wroclaw", "Diuna: Część druga", "diuna-czesc-druga"),
        )
    }

    @Test
    fun `slug link carries no query string or escapes`() {
        val url = filmShareUrl("https://kinowo.net", "warszawa", "Lilo & Stitch", "lilo-stitch")
        assertFalse(url.contains("?"))
        assertFalse(url.contains("%"))
    }

    @Test
    fun `falls back to the query form when the server sent no slug`() {
        // An older server leaves `slug` null; the query form still resolves
        // server-side (301 onto the slug address).
        assertEquals(
            "https://kinowo.net/poznan/movie?title=Oppenheimer",
            filmShareUrl("https://kinowo.net", "poznan", "Oppenheimer", null),
        )
        assertEquals(
            "https://kinowo.net/poznan/movie?title=Oppenheimer",
            filmShareUrl("https://kinowo.net", "poznan", "Oppenheimer", ""),
        )
    }

    @Test
    fun `plain ascii title is left intact`() {
        assertEquals("https://kinowo.net/poznan/movie?title=Oppenheimer", filmShareUrl("https://kinowo.net", "poznan", "Oppenheimer"))
    }

    @Test
    fun `carries the city slug in the path`() {
        assertEquals(
            "https://kinowo.net/bielsko-biala/movie?title=Oppenheimer",
            filmShareUrl("https://kinowo.net", "bielsko-biala", "Oppenheimer"),
        )
    }

    @Test
    fun `space and ampersand encode`() {
        assertEquals(
            "https://kinowo.net/warszawa/movie?title=Lilo%20%26%20Stitch",
            filmShareUrl("https://kinowo.net", "warszawa", "Lilo & Stitch"),
        )
    }

    @Test
    fun `colon and polish diacritics encode`() {
        assertEquals(
            "https://kinowo.net/wroclaw/movie?title=Diuna%3A%20Cz%C4%99%C5%9B%C4%87%20druga",
            filmShareUrl("https://kinowo.net", "wroclaw", "Diuna: Część druga"),
        )
    }

    @Test
    fun `never emits the form plus for a space`() {
        assertFalse(filmShareUrl("https://kinowo.net", "poznan", "Past Lives").contains("+"))
    }
}
