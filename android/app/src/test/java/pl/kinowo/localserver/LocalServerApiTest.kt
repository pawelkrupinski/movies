package pl.kinowo.localserver

import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Assume.assumeTrue
import org.junit.Before
import org.junit.Test
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.KinowoApi

/**
 * Android twin of the iOS `LocalServer` suite: hits a live boot of
 * `FixtureServerMain` (the same Play fixture corpus the page tests serve) over
 * HTTP and decodes its real `/api/repertoire` + `/api/details` JSON through the
 * production [KinowoApi] + kotlinx.serialization models.
 *
 * Why a live server instead of the recorded fixtures in [pl.kinowo.JsonDecodingTest]:
 * those snapshots only catch *client* drift; they go stale silently when the
 * server's JSON changes. Running against the live render closes that loop — a
 * `MovieController.apiRepertoire` shape change that breaks the Android decoder
 * fails CI on the same PR that introduced it (the `android-local-server` job in
 * deploy.yml boots the server and exports [KINOWO_LOCAL_URL]).
 *
 * Skipping when `KINOWO_LOCAL_URL` is unset is the correct default: a plain
 * `./gradlew testDebugUnitTest` (android.yml) shouldn't require sbt running, so
 * these are no-ops there and only execute in the dedicated job.
 */
class LocalServerApiTest {

    private lateinit var api: KinowoApi

    @Before
    fun setUp() {
        val base = System.getenv("KINOWO_LOCAL_URL")
        assumeTrue(
            "set KINOWO_LOCAL_URL to run LocalServer tests (boot FixtureServerMain)",
            !base.isNullOrBlank(),
        )
        api = KinowoApi(baseUrl = base!!)
    }

    @Test
    fun `repertoire decodes into Films with a full showings tree`() = runBlocking {
        val fetched = api.fetchRepertoire(citySlug = "poznan", ifModifiedSince = null)
        assertFalse("304 not expected on a fresh fetch", fetched.notModified)
        assertNotNull("the JSON API should stamp a Last-Modified", fetched.lastModified)

        val films = fetched.items!!
        assertTrue("expected a non-empty repertoire, got ${films.size}", films.size >= 20)
        assertTrue("every film should carry a non-blank title", films.all { it.title.isNotBlank() })

        // Posters present on the vast majority — same ≥80% bar the iOS home test uses.
        val withPoster = films.count { it.posterURL != null }
        assertTrue(
            "expected ≥80% films with a poster, got $withPoster/${films.size}",
            withPoster * 100 >= films.size * 80,
        )

        // The listing carries the full date → cinema → showtime tree, HH:MM times.
        val showtimes = films.flatMap { it.showings }.flatMap { it.cinemas }.flatMap { it.showtimes }
        assertTrue("expected at least one showtime across the corpus", showtimes.isNotEmpty())
        val hhmm = Regex("""\d{2}:\d{2}""")
        assertTrue("showtimes should be HH:MM", showtimes.all { hhmm.matches(it.time) })
    }

    @Test
    fun `cinema links derive from the live showings, deduped and sorted`() = runBlocking {
        val films = api.fetchRepertoire(citySlug = "poznan", ifModifiedSince = null).items!!
        val withLinks = films.firstOrNull { it.cinemaLinks.isNotEmpty() }
        assertNotNull("expected a film with derivable cinema links", withLinks)

        val cinemas = withLinks!!.cinemaLinks.map { it.cinema }
        assertTrue("link cinemas + urls should be non-blank", withLinks.cinemaLinks.all { it.cinema.isNotBlank() && it.url.isNotBlank() })
        assertEquals("links should be deduped by cinema", cinemas.distinct(), cinemas)
        assertEquals("links should be alpha-sorted by cinema", cinemas.sorted(), cinemas)
    }

    @Test
    fun `details decode synopsis and ready-to-embed trailers`() = runBlocking {
        val details = api.fetchDetails(citySlug = "poznan", ifModifiedSince = null).items!!
        assertTrue("expected a non-empty details payload", details.isNotEmpty())
        // The server only emits rows with content; nothing empty slips through.
        assertTrue(
            "every details row should carry synopsis, a trailer, or an original title",
            details.all { !it.synopsis.isNullOrBlank() || it.trailerURLs.isNotEmpty() || it.originalTitle != null },
        )
        val withTrailers = details.firstOrNull { it.trailerURLs.isNotEmpty() }
        assertNotNull("expected an entry with trailers", withTrailers)
        assertTrue(
            "trailer URLs should be embed-ready",
            withTrailers!!.trailerURLs.all { it.contains("/embed/") || it.contains("player.vimeo") },
        )
    }

    @Test
    fun `every details row matches a listed film by title`() = runBlocking {
        val films = api.fetchRepertoire(citySlug = "poznan", ifModifiedSince = null).items!!.associateBy(Film::title)
        val details = api.fetchDetails(citySlug = "poznan", ifModifiedSince = null).items!!
        // The app merges the two endpoints by title (DetailsRepository.associateBy);
        // an orphan details row would silently never reach the UI.
        val orphans = details.map(FilmDetails::title).filterNot(films::containsKey)
        assertTrue("details rows must match a listed film by title; orphans=$orphans", orphans.isEmpty())
    }
}
