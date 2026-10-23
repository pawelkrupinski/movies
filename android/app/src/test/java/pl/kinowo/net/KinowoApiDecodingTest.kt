package pl.kinowo.net

import kotlinx.coroutines.runBlocking
import kotlinx.serialization.decodeFromString
import okhttp3.OkHttpClient
import okhttp3.mockwebserver.MockResponse
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import pl.kinowo.contracts.MockWebServerRule
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.recordedFixture

/**
 * [KinowoApi] decodes the response body as it streams in rather than buffering
 * it whole into a String first; on the recorded `/api/repertoire` and
 * `/api/details` payloads that must yield exactly the models decoding the
 * whole document does.
 */
class KinowoApiDecodingTest {

    @get:Rule
    val mock = MockWebServerRule()
    private val api by lazy { KinowoApi(baseUrl = mock.baseUrl, client = OkHttpClient()) }

    @Test
    fun aStreamedRepertoireDecodesToTheSameFilms() = runBlocking {
        val body = recordedFixture("repertoire_sample.json")
        mock.server.enqueue(MockResponse().setBody(body))

        val fetched = api.fetchRepertoire("warszawa", ifModifiedSince = null).items

        assertEquals(LenientJson.decodeFromString<List<Film>>(body), fetched)
    }

    @Test
    fun streamedDetailsDecodeToTheSameDetails() = runBlocking {
        val body = recordedFixture("details_sample.json")
        mock.server.enqueue(MockResponse().setBody(body))

        val fetched = api.fetchDetails("warszawa", ifModifiedSince = null).items

        assertEquals(LenientJson.decodeFromString<List<FilmDetails>>(body), fetched)
    }
}
