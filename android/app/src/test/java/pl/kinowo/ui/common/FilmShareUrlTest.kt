package pl.kinowo.ui.common

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test

/**
 * `filmShareUrl` must mirror the server's `controllers.FilmHref`, so a link
 * shared from the app is byte-identical to one copied off the website. That is
 * now the city-scoped slug path `/<city>/film/<slug>`; the legacy `?title=`
 * form stays for films the server sent no slug for, and must keep its exact
 * encoding — spaces as `%20` (never the form `+`), reserved characters and
 * Polish diacritics percent-encoded.
 */
class FilmShareUrlTest {

    @Test
    fun `prefers the server-supplied slug`() {
        assertEquals(
            "https://kinowo.net/wroclaw/film/diuna-czesc-druga",
            filmShareUrl("wroclaw", "Diuna: Część druga", "diuna-czesc-druga"),
        )
    }

    @Test
    fun `slug link carries no query string or escapes`() {
        val url = filmShareUrl("warszawa", "Lilo & Stitch", "lilo-stitch")
        assertFalse(url.contains("?"))
        assertFalse(url.contains("%"))
    }

    @Test
    fun `falls back to the query form when the server sent no slug`() {
        // An older server leaves `slug` null; the query form still resolves
        // server-side (301 onto the slug address).
        assertEquals(
            "https://kinowo.net/poznan/film?title=Oppenheimer",
            filmShareUrl("poznan", "Oppenheimer", null),
        )
        assertEquals(
            "https://kinowo.net/poznan/film?title=Oppenheimer",
            filmShareUrl("poznan", "Oppenheimer", ""),
        )
    }

    @Test
    fun `plain ascii title is left intact`() {
        assertEquals("https://kinowo.net/poznan/film?title=Oppenheimer", filmShareUrl("poznan", "Oppenheimer"))
    }

    @Test
    fun `carries the city slug in the path`() {
        assertEquals(
            "https://kinowo.net/bielsko-biala/film?title=Oppenheimer",
            filmShareUrl("bielsko-biala", "Oppenheimer"),
        )
    }

    @Test
    fun `space and ampersand encode`() {
        assertEquals(
            "https://kinowo.net/warszawa/film?title=Lilo%20%26%20Stitch",
            filmShareUrl("warszawa", "Lilo & Stitch"),
        )
    }

    @Test
    fun `colon and polish diacritics encode`() {
        assertEquals(
            "https://kinowo.net/wroclaw/film?title=Diuna%3A%20Cz%C4%99%C5%9B%C4%87%20druga",
            filmShareUrl("wroclaw", "Diuna: Część druga"),
        )
    }

    @Test
    fun `never emits the form plus for a space`() {
        assertFalse(filmShareUrl("poznan", "Past Lives").contains("+"))
    }
}
