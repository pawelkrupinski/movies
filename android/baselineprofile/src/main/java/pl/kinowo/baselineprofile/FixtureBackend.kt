package pl.kinowo.baselineprofile

import androidx.test.platform.app.InstrumentationRegistry
import mockwebserver3.Dispatcher
import mockwebserver3.MockResponse
import mockwebserver3.MockWebServer
import mockwebserver3.RecordedRequest
import okio.Buffer
import java.net.InetAddress
import java.time.LocalDate
import java.time.ZoneId
import java.time.temporal.ChronoUnit

/**
 * The backend the profiled app talks to: a loopback server inside this test
 * process, which the app's benchmark build reaches through
 * `BenchmarkApplication` (every request, API and posters alike, rewritten to
 * [PORT]) — so a profile or a measurement never touches prod or the network.
 *
 * It serves the fixture corpus `tools.FixtureServerMain` (the web PageTest /
 * iOS LocalServer fixture server) answers for Poznań, captured into
 * `assets/fixtures/` with
 *
 *     scripts/sbt-warm --serve 'web/PageTest/runMain tools.FixtureServerMain <port-file>'
 *     curl http://127.0.0.1:$(cat <port-file>)/poznan/api/{repertoire,details,cinemas}
 *     curl http://127.0.0.1:$(cat <port-file>)/api/client-support
 *
 * The corpus is anchored on [CORPUS_DAY]; the app drops past showtimes against
 * the device clock, so every showing `date` is shifted by the days since then
 * and the grid fills as it would have that day. Every other path — a poster
 * on any host — gets one synthetic JPEG, so poster decoding runs too.
 */
class FixtureBackend : AutoCloseable {
    private val server = MockWebServer()

    fun start(): FixtureBackend = apply {
        server.dispatcher = FixtureDispatcher(shiftDays = ChronoUnit.DAYS.between(CORPUS_DAY, LocalDate.now(WARSAW)))
        server.start(InetAddress.getByName("127.0.0.1"), PORT)
    }

    override fun close() = server.close()

    private class FixtureDispatcher(private val shiftDays: Long) : Dispatcher() {
        private val repertoire = shiftShowingDates(asset("repertoire.json").utf8(), shiftDays)
        private val details = asset("details.json").utf8()
        private val cinemas = asset("cinemas.json").utf8()
        private val clientSupport = asset("client-support.json").utf8()
        private val poster = asset("poster.jpg")

        override fun dispatch(request: RecordedRequest): MockResponse = when (request.url.encodedPath) {
            "/poznan/api/repertoire" -> json(repertoire)
            "/poznan/api/details" -> json(details)
            "/poznan/api/cinemas" -> json(cinemas)
            "/api/client-support" -> json(clientSupport)
            // The catalog keeps its bundled seed (the fixture server has no
            // /api/catalog either); other cities have no corpus.
            "/api/catalog" -> MockResponse.Builder().code(404).build()
            else -> if (request.url.encodedPath.contains("/api/")) MockResponse.Builder().code(404).build()
            else MockResponse.Builder().addHeader("Content-Type", "image/jpeg").body(Buffer().write(poster)).build()
        }

        private fun json(body: String) =
            MockResponse.Builder().addHeader("Content-Type", "application/json; charset=UTF-8").body(body).build()
    }

    companion object {
        /** Must match `BenchmarkApplication.FIXTURE_ORIGIN` in :app's src/benchmark. */
        const val PORT = 8737
        val CORPUS_DAY: LocalDate = LocalDate.of(2026, 6, 8)
        private val WARSAW: ZoneId = ZoneId.of("Europe/Warsaw")
        private val SHOWING_DATE = Regex(""""date":"(\d{4}-\d{2}-\d{2})"""")

        internal fun shiftShowingDates(json: String, days: Long): String =
            SHOWING_DATE.replace(json) { """"date":"${LocalDate.parse(it.groupValues[1]).plusDays(days)}"""" }

        private fun asset(name: String) = InstrumentationRegistry.getInstrumentation().context.assets
            .open("fixtures/$name").use { okio.Buffer().readFrom(it).readByteString() }
    }
}
