package pl.kinowo.data

import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import pl.kinowo.model.Film
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.RepertoireApi

/**
 * Guards the city-switch bug: the backend stamps a single *global*
 * `Last-Modified` for every city, so replaying one city's timestamp as
 * `If-Modified-Since` while fetching another city draws a 304 — and the grid
 * used to stay frozen on the previous city's films. [JsonListCache] now binds
 * the cached timestamp to the city it came from, so a switch sends no
 * conditional header and gets a fresh 200.
 */
class RepertoireRepositoryTest {

    @get:Rule
    val tmp = TemporaryFolder()

    private fun cache() = JsonListCache(tmp.newFolder(), "repertoire", Film.serializer())

    /** A backend whose `Last-Modified` is a single global value, exactly like the
     *  real server (`MovieController.conditionalJson` reads one `movieCache.lastModified`
     *  regardless of city): any `If-Modified-Since` equal to it 304s, whatever city. */
    private class GlobalLastModifiedApi(
        private val byCity: Map<String, List<Film>>,
        private val lastModified: String,
    ) : RepertoireApi {
        var lastIfModifiedSince: String? = null
            private set

        override suspend fun fetchRepertoire(citySlug: String, ifModifiedSince: String?): KinowoApi.Fetched<Film> {
            lastIfModifiedSince = ifModifiedSince
            return if (ifModifiedSince == lastModified) {
                KinowoApi.Fetched(null, lastModified, notModified = true)
            } else {
                KinowoApi.Fetched(byCity[citySlug] ?: emptyList(), lastModified, notModified = false)
            }
        }
    }

    private val LM = "Mon, 01 Jun 2026 10:00:00 GMT"

    @Test
    fun `switching city replaces the grid despite the server's global Last-Modified`() = runBlocking {
        val poznan = listOf(Film(title = "Poznań film"))
        val warszawa = listOf(Film(title = "Warszawa film"))
        val api = GlobalLastModifiedApi(mapOf("poznan" to poznan, "warszawa" to warszawa), LM)
        val repo = RepertoireRepository(api, cache())

        repo.reload("poznan")
        assertEquals(poznan, repo.films.value)

        // Before the fix this sent poznań's (global) Last-Modified, drew a 304,
        // and the grid stayed on the Poznań films.
        repo.reload("warszawa")
        assertEquals(warszawa, repo.films.value)
    }

    @Test
    fun `reloading the same city still revalidates with If-Modified-Since`() = runBlocking {
        val poznan = listOf(Film(title = "Poznań film"))
        val api = GlobalLastModifiedApi(mapOf("poznan" to poznan), LM)
        val repo = RepertoireRepository(api, cache())

        repo.reload("poznan")                       // 200 — stores the timestamp for Poznań
        assertEquals(null, api.lastIfModifiedSince)

        repo.reload("poznan")                       // same city — must revalidate
        assertEquals(LM, api.lastIfModifiedSince)   // conditional header replayed
        assertEquals(poznan, repo.films.value)      // 304 keeps the (correct) cached city
    }
}
