package pl.kinowo.ui.common

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test

/**
 * `filmShareUrl` must mirror the server's `controllers.FilmHref`: the
 * city-scoped path `/<city>/film?title=…` (a city-less `/film?title=…` has no
 * server route and 404s), and the same encoding so a link shared from the app
 * is byte-identical to one copied off the website — spaces as `%20` (never the
 * form `+`), and every reserved character or Polish diacritic percent-encoded.
 */
class FilmShareUrlTest {

    @Test
    fun `plain ascii title is left intact`() {
        assertEquals("https://kinowo.fly.dev/poznan/film?title=Oppenheimer", filmShareUrl("poznan", "Oppenheimer"))
    }

    @Test
    fun `carries the city slug in the path`() {
        assertEquals(
            "https://kinowo.fly.dev/bielsko-biala/film?title=Oppenheimer",
            filmShareUrl("bielsko-biala", "Oppenheimer"),
        )
    }

    @Test
    fun `space and ampersand encode`() {
        assertEquals(
            "https://kinowo.fly.dev/warszawa/film?title=Lilo%20%26%20Stitch",
            filmShareUrl("warszawa", "Lilo & Stitch"),
        )
    }

    @Test
    fun `colon and polish diacritics encode`() {
        assertEquals(
            "https://kinowo.fly.dev/wroclaw/film?title=Diuna%3A%20Cz%C4%99%C5%9B%C4%87%20druga",
            filmShareUrl("wroclaw", "Diuna: Część druga"),
        )
    }

    @Test
    fun `never emits the form plus for a space`() {
        assertFalse(filmShareUrl("poznan", "Past Lives").contains("+"))
    }
}
