package pl.kinowo.auth

import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import okhttp3.mockwebserver.MockResponse
import okhttp3.mockwebserver.MockWebServer
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Before
import org.junit.Test

/**
 * Pins the URL/header shape [HttpHiddenFilmsClient] builds, against a real
 * MockWebServer — same pattern as `net.KinowoApiPathTest`. `title` travels as
 * a URL PATH segment (not a query value), and a space, an embedded `/`, or
 * non-ASCII text is exactly the kind of thing that silently double-encodes or
 * mis-splits the one time it matters; asserting on the server's OWN decoded
 * `path`/`requestUrl` proves the real behaviour, not just what the code looks
 * like it does.
 */
class HiddenFilmsClientPathTest {

    private lateinit var server: MockWebServer
    private lateinit var client: HttpHiddenFilmsClient

    @Before
    fun setUp() {
        server = MockWebServer()
        server.start()
        client = HttpHiddenFilmsClient(baseUrl = server.url("").toString().trimEnd('/'), client = OkHttpClient())
    }

    @After
    fun tearDown() {
        server.shutdown()
    }

    @Test
    fun fetchPathCarriesTheCountry() = runBlocking {
        server.enqueue(MockResponse().setBody("""{"hiddenFilms":[]}"""))
        client.fetch("pl", null, null)
        assertEquals("/api/me/pl/hidden-films", server.takeRequest().path)
    }

    @Test
    fun hidePathEncodesASpaceAsPercentTwenty_notPlus() = runBlocking {
        server.enqueue(MockResponse().setBody("""{"hiddenFilms":["Mission Impossible"]}"""))
        client.hide("us", "Mission Impossible")
        // %20, not the query-string "+" a plain URLEncoder would produce.
        assertEquals("/api/me/us/hidden-films/Mission%20Impossible", server.takeRequest().path)
    }

    @Test
    fun hidePathEncodesAnEmbeddedSlashSoItSurvivesAsOneSegment() = runBlocking {
        server.enqueue(MockResponse().setBody("""{"hiddenFilms":[]}"""))
        client.hide("pl", "S/He")
        val request = server.takeRequest()
        // The server's own router must see ONE path segment for the title, not
        // an extra one from an un-encoded "/" — MockWebServer's decoded path
        // proves the literal slash survived as %2F rather than splitting the URL.
        assertEquals("/api/me/pl/hidden-films/S%2FHe", request.path)
    }

    @Test
    fun hidePathEncodesNonAsciiCharacters() = runBlocking {
        server.enqueue(MockResponse().setBody("""{"hiddenFilms":[]}"""))
        client.hide("pl", "Diabeł ubiera się u Prady 2")
        val request = server.takeRequest()
        assertEquals("Diabeł ubiera się u Prady 2", java.net.URLDecoder.decode(request.path!!.substringAfterLast('/'), "UTF-8"))
    }

    @Test
    fun unhideUsesDelete() = runBlocking {
        server.enqueue(MockResponse().setBody("""{"hiddenFilms":[]}"""))
        client.unhide("pl", "Sing")
        val request = server.takeRequest()
        assertEquals("DELETE", request.method)
        assertEquals("/api/me/pl/hidden-films/Sing", request.path)
    }

    @Test
    fun clearPathHasNoTitleSegment() = runBlocking {
        server.enqueue(MockResponse().setBody("""{"hiddenFilms":[]}"""))
        client.clear("pl")
        val request = server.takeRequest()
        assertEquals("DELETE", request.method)
        assertEquals("/api/me/pl/hidden-films", request.path)
    }

    @Test
    fun fetchSendsIfNoneMatchWhenAnEtagIsStored() = runBlocking {
        server.enqueue(MockResponse().setBody("""{"hiddenFilms":[]}"""))
        client.fetch("pl", "\"abc123\"", "Tue, 19 May 2026 12:00:00 GMT")
        val request = server.takeRequest()
        assertEquals("\"abc123\"", request.getHeader("If-None-Match"))
        // If-None-Match is authoritative when present (RFC 7232 §3.3) — the
        // date-based validator must not ALSO be sent.
        assertNull(request.getHeader("If-Modified-Since"))
    }

    @Test
    fun fetchFallsBackToIfModifiedSinceWithNoStoredEtag() = runBlocking {
        server.enqueue(MockResponse().setBody("""{"hiddenFilms":[]}"""))
        client.fetch("pl", null, "Tue, 19 May 2026 12:00:00 GMT")
        val request = server.takeRequest()
        assertNull(request.getHeader("If-None-Match"))
        assertEquals("Tue, 19 May 2026 12:00:00 GMT", request.getHeader("If-Modified-Since"))
    }

    @Test
    fun notModifiedResponseIsRecognised() = runBlocking {
        server.enqueue(MockResponse().setResponseCode(304))
        val result = client.fetch("pl", "\"abc123\"", null)
        assertEquals(HiddenFilmsFetchResult.NotModified, result)
    }

    @Test
    fun changedResponseCarriesTheFreshValidators() = runBlocking {
        server.enqueue(
            MockResponse()
                .setBody("""{"hiddenFilms":["Sing"]}""")
                .setHeader("ETag", "\"new-etag\"")
                .setHeader("Last-Modified", "Wed, 20 May 2026 09:00:00 GMT")
        )
        val result = client.fetch("pl", null, null) as HiddenFilmsFetchResult.Changed
        assertEquals(setOf("Sing"), result.state.hiddenFilms)
        assertEquals("\"new-etag\"", result.state.etag)
        assertEquals("Wed, 20 May 2026 09:00:00 GMT", result.state.lastModified)
    }
}
