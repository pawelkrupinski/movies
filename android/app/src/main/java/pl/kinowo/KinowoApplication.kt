package pl.kinowo

import android.app.Application
import coil3.ImageLoader
import coil3.PlatformContext
import coil3.SingletonImageLoader
import coil3.annotation.ExperimentalCoilApi
import coil3.network.cachecontrol.CacheControlCacheStrategy
import coil3.network.okhttp.OkHttpNetworkFetcherFactory
import okhttp3.CookieJar
import okhttp3.OkHttpClient
import pl.kinowo.model.Country
import pl.kinowo.net.PersistentCookieJar
import java.util.concurrent.TimeUnit

/**
 * The process-wide half of the composition root: the ONE OkHttp client every
 * caller shares — the API, auth and sync clients through [viewModelFactory],
 * and Coil's poster loader through [newImageLoader] — so they share one
 * connection pool and dispatcher instead of Coil building a second client of
 * its own. Per-country wiring stays in [kinowoViewModelFactory].
 */
open class KinowoApplication : Application(), KinowoGraphProvider, SingletonImageLoader.Factory {

    // A disk-backed jar so the signed-in session cookie survives restarts.
    private val cookieJar by lazy { PersistentCookieJar(this) }

    internal val httpClient: OkHttpClient by lazy { httpClientBuilder().build() }

    /** The shared client's configuration. Open for the Baseline Profile /
     *  Macrobenchmark builds only (`src/benchmark`'s BenchmarkApplication), which
     *  add an interceptor pointing every request at the on-device fixture server. */
    protected open fun httpClientBuilder(): OkHttpClient.Builder = OkHttpClient.Builder()
        .cookieJar(cookieJar)
        .connectTimeout(15, TimeUnit.SECONDS)
        .readTimeout(20, TimeUnit.SECONDS)

    override fun viewModelFactory(country: Country) = kinowoViewModelFactory(this, country, httpClient, cookieJar)

    // `newBuilder()` keeps the shared pool and dispatcher; only the cookie jar
    // differs: poster requests go to cinema CDNs, whose cookies have no place in
    // the persisted session jar. Coil's disk and memory caches keep their
    // defaults (PosterPrefetch and the poster purge rely on them). The
    // Cache-Control strategy keeps Coil 2's behaviour of honouring the CDNs'
    // caching headers, which Coil 3 otherwise ignores.
    @OptIn(ExperimentalCoilApi::class) // CacheControlCacheStrategy
    override fun newImageLoader(context: PlatformContext): ImageLoader = ImageLoader.Builder(context)
        .components {
            add(
                OkHttpNetworkFetcherFactory(
                    callFactory = { httpClient.newBuilder().cookieJar(CookieJar.NO_COOKIES).build() },
                    cacheStrategy = { CacheControlCacheStrategy() },
                )
            )
        }
        .build()
}
