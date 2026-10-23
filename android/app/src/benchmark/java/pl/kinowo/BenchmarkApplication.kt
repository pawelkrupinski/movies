package pl.kinowo

import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.Interceptor
import okhttp3.OkHttpClient

/**
 * The Application of the Baseline Profile / Macrobenchmark builds: the real
 * [KinowoApplication] with every request — the API on kinowo.net / showtimes.cc
 * and every poster CDN alike — re-pointed at the fixture server the
 * `:baselineprofile` module runs on the device ([FIXTURE_ORIGIN]), so profiling
 * never touches prod and the measured journey sees the same corpus every run.
 * The original host travels in [ORIGINAL_HOST_HEADER] for the server to route on.
 */
class BenchmarkApplication : KinowoApplication() {
    override fun httpClientBuilder(): OkHttpClient.Builder =
        super.httpClientBuilder().addInterceptor(RedirectToFixtureServer)

    private object RedirectToFixtureServer : Interceptor {
        private val origin = FIXTURE_ORIGIN.toHttpUrl()

        override fun intercept(chain: Interceptor.Chain) = chain.request().let { request ->
            chain.proceed(
                request.newBuilder()
                    .url(request.url.newBuilder().scheme(origin.scheme).host(origin.host).port(origin.port).build())
                    .header(ORIGINAL_HOST_HEADER, request.url.host)
                    .build()
            )
        }
    }

    companion object {
        /** Must match `FixtureBackend.PORT` in the :baselineprofile module. */
        const val FIXTURE_ORIGIN = "http://127.0.0.1:8737"
        const val ORIGINAL_HOST_HEADER = "X-Kinowo-Original-Host"
    }
}
