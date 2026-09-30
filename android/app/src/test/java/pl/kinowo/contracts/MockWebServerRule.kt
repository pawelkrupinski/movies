package pl.kinowo.contracts

import okhttp3.mockwebserver.MockWebServer
import org.junit.rules.ExternalResource

/** A [MockWebServer] started before each test and shut down after it, with the
 *  [baseUrl] a client under test is pointed at (no trailing slash). */
class MockWebServerRule : ExternalResource() {
    val server = MockWebServer()

    val baseUrl: String get() = server.url("").toString().trimEnd('/')

    override fun before() = server.start()

    override fun after() = server.shutdown()
}
