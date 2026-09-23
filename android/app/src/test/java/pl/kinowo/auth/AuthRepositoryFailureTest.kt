package pl.kinowo.auth

import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import okhttp3.mockwebserver.MockResponse
import okhttp3.mockwebserver.MockWebServer
import org.junit.After
import org.junit.Assert.assertNull
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.net.PersistentCookieJar

/**
 * [AuthRepository.exchangeCode] runs in a bare `viewModelScope.launch` (the
 * `kinowo://auth-done` redirect), where an escaping exception crashes the app.
 * So a flaky network or an unexpected body on the way back from the OAuth
 * consent screen must leave the user signed out, not kill the process — the
 * same contract [AuthRepository.checkSession] already honoured.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class AuthRepositoryFailureTest {

    private lateinit var server: MockWebServer

    @Before
    fun setUp() {
        server = MockWebServer()
        server.start()
    }

    @After
    fun tearDown() {
        server.shutdown()
    }

    private fun repository(baseUrl: String) = AuthRepository(
        OkHttpClient(),
        PersistentCookieJar(ApplicationProvider.getApplicationContext()),
        baseUrl,
    )

    @Test
    fun exchangeCodeSurvivesAnUnreachableServer() = runBlocking {
        val auth = repository("http://127.0.0.1:1")

        auth.exchangeCode("one-shot")

        assertNull(auth.user.value)
    }

    @Test
    fun exchangeCodeSurvivesAMalformedBody() = runBlocking {
        server.enqueue(MockResponse().setBody("<html>proxy error</html>"))
        val auth = repository(server.url("").toString().trimEnd('/'))

        auth.exchangeCode("one-shot")

        assertNull(auth.user.value)
    }
}
