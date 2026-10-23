package pl.kinowo

import android.app.Application
import coil.ImageLoader
import coil.ImageLoaderFactory
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
open class KinowoApplication : Application(), KinowoGraphProvider, ImageLoaderFactory {

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
    // defaults (PosterPrefetch and the poster purge rely on them).
    override fun newImageLoader(): ImageLoader = ImageLoader.Builder(this)
        .okHttpClient { httpClient.newBuilder().cookieJar(CookieJar.NO_COOKIES).build() }
        .build()
}
