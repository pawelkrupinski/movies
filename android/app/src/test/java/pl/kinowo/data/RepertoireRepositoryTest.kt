package pl.kinowo.data

import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.launch
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.yield
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
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
        val repository = RepertoireRepository(api, cache())

        repository.reload("poznan")
        assertEquals(poznan, repository.films.value)

        // Before the fix this sent poznań's (global) Last-Modified, drew a 304,
        // and the grid stayed on the Poznań films.
        repository.reload("warszawa")
        assertEquals(warszawa, repository.films.value)
    }

    @Test
    fun `loadedCity tracks the city whose films are held, across a switch`() = runBlocking {
        val poznan = listOf(Film(title = "Poznań film"))
        val warszawa = listOf(Film(title = "Warszawa film"))
        val api = GlobalLastModifiedApi(mapOf("poznan" to poznan, "warszawa" to warszawa), LM)
        val repository = RepertoireRepository(api, cache())

        assertEquals(null, repository.loadedCity.value)   // nothing loaded yet
        repository.reload("poznan")
        assertEquals("poznan", repository.loadedCity.value)
        repository.reload("warszawa")
        // The deep-link gate keys on this: it stays "poznan" until warszawa's
        // load actually lands, so a film lookup never runs against stale films.
        assertEquals("warszawa", repository.loadedCity.value)
    }

    /** An API whose response for the NEXT call waits until [release] is
     *  called, so a test can inspect repository state mid-fetch — the fake
     *  itself never resolves the target city until the test says so. */
    private class GatedApi(private val byCity: Map<String, List<Film>>) : RepertoireApi {
        private var gate = CompletableDeferred<Unit>()

        fun release() {
            gate.complete(Unit)
        }

        override suspend fun fetchRepertoire(citySlug: String, ifModifiedSince: String?): KinowoApi.Fetched<Film> {
            gate.await()
            gate = CompletableDeferred() // re-arm for the next call
            return KinowoApi.Fetched(byCity[citySlug] ?: emptyList(), null, notModified = false)
        }
    }

    @Test
    fun `switching city drops the outgoing city's films while the new fetch is in flight`() = runBlocking {
        val poznan = listOf(Film(title = "Poznań film"))
        val warszawa = listOf(Film(title = "Warszawa film"))
        val api = GatedApi(mapOf("poznan" to poznan, "warszawa" to warszawa))
        val repository = RepertoireRepository(api, cache())

        api.release() // let the first (poznan) fetch through immediately
        repository.reload("poznan")
        assertEquals(poznan, repository.films.value)

        // warszawa's fetch is gated — reload() suspends on it, so its FIRST
        // (synchronous) act — dropping poznań's films — is what's observable
        // right now, before any network response has arrived.
        val job = launch { repository.reload("warszawa") }
        yield() // let the child coroutine run up to the gate

        assertEquals(
            "the previous city's films should be gone the moment a real switch starts, not linger until the new city's fetch resolves",
            emptyList<Film>(), repository.films.value,
        )
        assertNull(repository.loadedCity.value)

        api.release()
        job.join()
        assertEquals(warszawa, repository.films.value)
        assertEquals("warszawa", repository.loadedCity.value)
    }

    @Test
    fun `a same-city refresh (foreground restale, pull-to-refresh) leaves films alone mid-flight`() = runBlocking {
        val poznan = listOf(Film(title = "Poznań film"))
        val api = GatedApi(mapOf("poznan" to poznan))
        val repository = RepertoireRepository(api, cache())

        api.release()
        repository.reload("poznan")
        assertEquals(poznan, repository.films.value)

        // Same city again — a refresh, not a switch. No reason to blank the
        // grid the user is already looking at while it revalidates.
        val job = launch { repository.reload("poznan") }
        yield()

        assertEquals(poznan, repository.films.value)
        assertEquals("poznan", repository.loadedCity.value)

        api.release()
        job.join()
    }

    @Test
    fun `reloading the same city still revalidates with If-Modified-Since`() = runBlocking {
        val poznan = listOf(Film(title = "Poznań film"))
        val api = GlobalLastModifiedApi(mapOf("poznan" to poznan), LM)
        val repository = RepertoireRepository(api, cache())

        repository.reload("poznan")                       // 200 — stores the timestamp for Poznań
        assertEquals(null, api.lastIfModifiedSince)

        repository.reload("poznan")                       // same city — must revalidate
        assertEquals(LM, api.lastIfModifiedSince)   // conditional header replayed
        assertEquals(poznan, repository.films.value)      // 304 keeps the (correct) cached city
    }
}
