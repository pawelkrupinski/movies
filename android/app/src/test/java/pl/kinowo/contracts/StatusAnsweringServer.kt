package pl.kinowo.contracts

import okhttp3.mockwebserver.Dispatcher
import okhttp3.mockwebserver.MockResponse
import okhttp3.mockwebserver.MockWebServer
import okhttp3.mockwebserver.RecordedRequest

/** Answer EVERY request with [status] until told otherwise — not one queued
 *  response per call: OkHttp silently resends a request answered 408, and a
 *  queued response per call would hand that resend the next row's status and
 *  leave the last call waiting out the read timeout. */
fun MockWebServer.answerEveryRequestWith(status: Int) {
    dispatcher = object : Dispatcher() {
        override fun dispatch(request: RecordedRequest) = MockResponse().setResponseCode(status)
    }
}
