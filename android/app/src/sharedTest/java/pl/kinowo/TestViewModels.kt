package pl.kinowo

import android.content.Context
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import okhttp3.OkHttpClient
import pl.kinowo.auth.AuthRepository
import pl.kinowo.auth.SharedPrefsPendingVerifierStore
import pl.kinowo.auth.StateSync
import pl.kinowo.data.CatalogCache
import pl.kinowo.data.CatalogRepository
import pl.kinowo.data.ClientSupportGate
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.location.GrantedLocationSource
import pl.kinowo.model.CinemaCatalog
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.CatalogApi
import pl.kinowo.net.CinemaCatalogApi
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.PersistentCookieJar
import pl.kinowo.ui.KinowoViewModel
import java.io.IOException
import pl.kinowo.net.TEST_USER_AGENT

// Shared by the JVM (`src/test`) and on-device (`src/androidTest`) suites —
// see the `sharedTest` source set in build.gradle.kts.

/** Every endpoint of a test ViewModel: a closed local port, so nothing a
 *  ViewModel does on its own (session check, fetches) ever reaches prod. */
const val UNREACHABLE_BASE_URL = "http://127.0.0.1:1"

/** A [StateSync] that does nothing: a test ViewModel never mirrors to a server. */
object NoopStateSync : StateSync {
    override fun start() {}
    override suspend fun reconcileCurrentCountry() {}
    override fun hide(country: String, title: String) {}
    override fun unhide(country: String, title: String) {}
    override fun clear(country: String) {}
}

/** A [ClientSupportGate] whose server is unreachable — so, by the gate's
 *  never-lock-out rule, the app stays usable. */
fun offlineClientSupportGate() = ClientSupportGate({ throw IOException("offline") }, appVersion = "1.0")

/**
 * A real [KinowoViewModel] wired for tests: offline, flat cinema catalog,
 * fallback-only country catalog (no seed, never revalidates), no server
 * sync, no location fix. Pass [repository] to seed the listing a test needs,
 * [sync] / [location] to observe or steer those collaborators, [catalogSeed]
 * (a bundled-seed envelope) for a country catalog other than the fallback,
 * [clientSupportGate] to have the server answer that this version is unsupported
 * (the default's fetch fails, so the app stays usable).
 */
fun testKinowoViewModel(
    context: Context,
    repository: RepertoireRepository? = null,
    prefs: UserPreferences = UserPreferences(context),
    sync: StateSync = NoopStateSync,
    location: GrantedLocationSource = GrantedLocationSource { null },
    http: OkHttpClient = OkHttpClient(),
    catalogSeed: String? = null,
    clientSupportGate: ClientSupportGate = offlineClientSupportGate(),
): KinowoViewModel {
    val api = KinowoApi(baseUrl = UNREACHABLE_BASE_URL, client = http, userAgent = TEST_USER_AGENT)
    return KinowoViewModel(
        repository = repository ?: RepertoireRepository(api, JsonListCache(context.cacheDir, "repertoire", Film.serializer())),
        detailsRepository = DetailsRepository(api, JsonListCache(context.cacheDir, "details", FilmDetails.serializer())),
        prefs = prefs,
        authRepository = AuthRepository(http, PersistentCookieJar(context), baseUrl = UNREACHABLE_BASE_URL,
            pendingVerifier = SharedPrefsPendingVerifierStore(context), userAgent = TEST_USER_AGENT),
        sync = sync,
        catalogApi = CinemaCatalogApi { CinemaCatalog.EMPTY },
        catalogRepository = CatalogRepository(
            api = CatalogApi { KinowoApi.FetchedCatalog(null, null, notModified = true) },
            cache = CatalogCache(java.io.File(context.cacheDir, "test-catalog")),
            seedJson = catalogSeed,
        ),
        location = location,
        clientSupportGate = clientSupportGate,
        scope = CoroutineScope(SupervisorJob() + Dispatchers.Main.immediate),
    )
}
