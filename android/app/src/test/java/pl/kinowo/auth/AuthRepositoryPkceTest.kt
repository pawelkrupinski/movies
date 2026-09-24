package pl.kinowo.auth

import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.runBlocking
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.OkHttpClient
import okhttp3.mockwebserver.MockResponse
import okhttp3.mockwebserver.MockWebServer
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotEquals
import org.junit.Assert.assertNotNull
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.net.PersistentCookieJar

/**
 * The native sign-in's PKCE-style verifier: the start URL carries the S256
 * challenge of a verifier the app keeps, `/auth/exchange` is sent that
 * verifier, and a deep-link code arriving with no sign-in in flight — a link
 * somebody sent to sign this app into THEIR account — is never presented.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class AuthRepositoryPkceTest {

    private lateinit var server: MockWebServer

    @Before fun setUp() { server = MockWebServer().also { it.start() } }
    @After fun tearDown() { server.shutdown() }

    private fun repository() = AuthRepository(
        OkHttpClient(),
        PersistentCookieJar(ApplicationProvider.getApplicationContext()),
        server.url("").toString().trimEnd('/'),
        SharedPrefsPendingVerifierStore(ApplicationProvider.getApplicationContext()),
    )

    @Test
    fun challengeIsTheRfc7636S256OfTheVerifier() {
        // RFC 7636 appendix B — the same vector the server's spec uses.
        assertEquals("E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM",
            PkceVerifier.challenge("dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"))
    }

    @Test
    fun verifiersAreFreshAndServerShaped() {
        val first = PkceVerifier.newVerifier()
        assertEquals(43, first.length)
        assertEquals(true, Regex("[A-Za-z0-9_-]{43}").matches(first))
        assertNotEquals(first, PkceVerifier.newVerifier())
    }

    @Test
    fun exchangePresentsTheVerifierBehindTheStartUrlsChallenge() = runBlocking {
        val auth = repository()
        val challenge = auth.webSignInUrl("google").toHttpUrl().queryParameter("challenge")
        server.enqueue(MockResponse().setBody("""{"displayName":"A","email":"a@x","avatarUrl":null,"provider":"google"}"""))

        auth.exchangeCode("one-shot")

        val body = Json.parseToJsonElement(server.takeRequest().body.readUtf8()).jsonObject
        assertEquals("one-shot", body["code"]!!.jsonPrimitive.content)
        assertEquals(challenge, PkceVerifier.challenge(body["verifier"]!!.jsonPrimitive.content))
        assertNotNull(auth.user.value)
    }

    @Test
    fun aCodeWithNoSignInInFlightIsNeverPresented() = runBlocking {
        val auth = repository()
        auth.webSignInUrl("google")
        server.enqueue(MockResponse().setBody("{}"))
        auth.exchangeCode("mine")                  // spends the pending verifier
        server.takeRequest()

        auth.exchangeCode("sent-by-somebody-else") // nothing in flight any more

        assertEquals(1, server.requestCount)
    }
}
