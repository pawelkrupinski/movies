package pl.kinowo.data

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.model.Film

class PosterCachePurgeTest {

    private fun film(title: String, poster: String?, fallbacks: List<String> = emptyList()) =
        Film(title = title, posterURL = poster, fallbackPosterURLs = fallbacks)

    @Test
    fun `keepUrls collects primary and fallback urls across films, deduped`() {
        val films = listOf(
            film("A", "a.jpg", listOf("a-alt.jpg")),
            film("B", "b.jpg"),
            film("C", null, listOf("c-alt.jpg", "a-alt.jpg")), // shares a-alt with A
        )
        assertEquals(
            setOf("a.jpg", "a-alt.jpg", "b.jpg", "c-alt.jpg"),
            PosterCachePurge.keepUrls(films),
        )
    }

    @Test
    fun `keepUrls skips films with no poster`() {
        assertEquals(emptySet<String>(), PosterCachePurge.keepUrls(listOf(film("A", null))))
    }

    @Test
    fun `toEvict drops departed films' posters and keeps current ones`() {
        // Yesterday the cache held posters for A, B and C.
        val seen = setOf("a.jpg", "b.jpg", "c.jpg")
        // Today only A and B still screen.
        val current = listOf(film("A", "a.jpg"), film("B", "b.jpg"))
        val keep = PosterCachePurge.keepUrls(current)

        val evict = PosterCachePurge.toEvict(seen, keep)

        assertEquals(setOf("c.jpg"), evict)
        assertTrue("a still-screening poster is never evicted", "a.jpg" !in evict)
    }

    @Test
    fun `toEvict drops a url a still-present film rotated away from`() {
        // A is still showing but its cinema swapped the poster URL.
        val seen = setOf("a-old.jpg", "b.jpg")
        val current = listOf(film("A", "a-new.jpg"), film("B", "b.jpg"))

        val evict = PosterCachePurge.toEvict(seen, PosterCachePurge.keepUrls(current))

        assertEquals(setOf("a-old.jpg"), evict)
    }

    @Test
    fun `toEvict is empty on the first run when nothing was seen yet`() {
        val current = listOf(film("A", "a.jpg"))
        assertTrue(PosterCachePurge.toEvict(emptySet(), PosterCachePurge.keepUrls(current)).isEmpty())
    }
}
