package pl.kinowo

import org.junit.Assert.assertEquals
import org.junit.Test
import pl.kinowo.ui.common.prefetchOrder

class PrefetchOrderTest {

    @Test fun `top anchor walks the list top-down`() {
        val urls = listOf("a", "b", "c", "d")
        assertEquals(listOf(0, 1, 2, 3), prefetchOrder(urls, anchor = 0))
    }

    @Test fun `mid anchor radiates outward, nearer indices first`() {
        val urls = listOf("a", "b", "c", "d", "e")
        // anchor 2: distances 2,1,0,1,2 → 2 first, then the ties 1 & 3
        // (stable: lower index first), then 0 & 4.
        assertEquals(listOf(2, 1, 3, 0, 4), prefetchOrder(urls, anchor = 2))
    }

    @Test fun `blank header slots are skipped but keep indices aligned`() {
        // Mirrors a CinemaGrid: header (blank), two films, header, one film.
        val urls = listOf("", "f0", "f1", "", "f2")
        // Indices of real posters are 1, 2, 4. anchor 1 → distances 0,1,3.
        assertEquals(listOf(1, 2, 4), prefetchOrder(urls, anchor = 1))
    }

    @Test fun `all-blank input yields nothing`() {
        assertEquals(emptyList<Int>(), prefetchOrder(listOf("", ""), anchor = 0))
    }
}
