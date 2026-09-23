package pl.kinowo

import android.content.Context
import okhttp3.OkHttpClient
import pl.kinowo.auth.AuthRepository
import pl.kinowo.auth.HiddenFilmsClient
import pl.kinowo.auth.HiddenFilmsFetchResult
import pl.kinowo.auth.HiddenFilmsState
import pl.kinowo.auth.LanguageClient
import pl.kinowo.data.CatalogCache
import pl.kinowo.data.CatalogRepository
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.model.CinemaCatalog
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.CatalogApi
import pl.kinowo.net.CinemaCatalogApi
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.PersistentCookieJar
import pl.kinowo.ui.KinowoViewModel

// Shared by the JVM (`src/test`) and on-device (`src/androidTest`) suites —
// see the `sharedTest` source set in build.gradle.kts.

/** Every endpoint of a test ViewModel: a closed local port, so nothing a
 *  ViewModel does on its own (session check, fetches) ever reaches prod. */
const val UNREACHABLE_BASE_URL = "http://127.0.0.1:1"

/** A [HiddenFilmsClient] that never touches the network and never changes anything. */
object NoopHiddenFilmsClient : HiddenFilmsClient {
    override suspend fun fetch(country: String, etag: String?, lastModified: String?) =
        HiddenFilmsFetchResult.NotModified
    override suspend fun hide(country: String, title: String) = HiddenFilmsState(emptySet(), null, null)
    override suspend fun unhide(country: String, title: String) = HiddenFilmsState(emptySet(), null, null)
    override suspend fun clear(country: String) = HiddenFilmsState(emptySet(), null, null)
}

/** No account language, and pushes go nowhere. */
object NoopLanguageClient : LanguageClient {
    override suspend fun fetch(): String? = null
    override suspend fun push(language: String) {}
}

/**
 * A real [KinowoViewModel] wired for tests: offline, flat cinema catalog,
 * fallback-only country catalog (no seed, never revalidates). Pass
 * [repository] to seed the listing a test needs.
 */
fun testKinowoViewModel(
    context: Context,
    repository: RepertoireRepository? = null,
    prefs: UserPreferences = UserPreferences(context),
    hiddenFilmsClient: HiddenFilmsClient = NoopHiddenFilmsClient,
    http: OkHttpClient = OkHttpClient(),
): KinowoViewModel {
    val api = KinowoApi(baseUrl = UNREACHABLE_BASE_URL, client = http)
    return KinowoViewModel(
        repository = repository ?: RepertoireRepository(api, JsonListCache(context.cacheDir, "repertoire", Film.serializer())),
        detailsRepository = DetailsRepository(api, JsonListCache(context.cacheDir, "details", FilmDetails.serializer())),
        prefs = prefs,
        authRepository = AuthRepository(http, PersistentCookieJar(context), baseUrl = UNREACHABLE_BASE_URL),
        hiddenFilmsClient = hiddenFilmsClient,
        languageClient = NoopLanguageClient,
        catalogApi = CinemaCatalogApi { CinemaCatalog.EMPTY },
        catalogRepository = CatalogRepository(
            api = CatalogApi { KinowoApi.FetchedCatalog(null, null, notModified = true) },
            cache = CatalogCache(java.io.File(context.cacheDir, "test-catalog")),
            seedJson = null,
        ),
    )
}
