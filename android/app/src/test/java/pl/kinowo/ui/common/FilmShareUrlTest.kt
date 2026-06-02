package pl.kinowo.ui.common

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test

/**
 * `filmShareUrl` must mirror the server's `controllers.FilmHref` encoding so
 * a link shared from the app is byte-identical to one copied off the website:
 * spaces as `%20` (never the form `+`), and every reserved character or Polish
 * diacritic percent-encoded.
 */
class FilmShareUrlTest {

    @Test
    fun `plain ascii title is left intact`() {
        assertEquals("https://kinowo.fly.dev/film?title=Oppenheimer", filmShareUrl("Oppenheimer"))
    }

    @Test
    fun `space and ampersand encode`() {
        assertEquals(
            "https://kinowo.fly.dev/film?title=Lilo%20%26%20Stitch",
            filmShareUrl("Lilo & Stitch"),
        )
    }

    @Test
    fun `colon and polish diacritics encode`() {
        assertEquals(
            "https://kinowo.fly.dev/film?title=Diuna%3A%20Cz%C4%99%C5%9B%C4%87%20druga",
            filmShareUrl("Diuna: Część druga"),
        )
    }

    @Test
    fun `never emits the form plus for a space`() {
        assertFalse(filmShareUrl("Past Lives").contains("+"))
    }
}
