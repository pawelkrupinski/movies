package pl.kinowo

import android.content.Context
import androidx.activity.ComponentActivity
import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.hasTestTag
import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithTag
import androidx.test.core.app.ApplicationProvider
import androidx.test.ext.junit.runners.AndroidJUnit4
import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import pl.kinowo.auth.AuthRepository
import pl.kinowo.auth.UserStateClient
import pl.kinowo.auth.UserSyncState
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.model.Showtime
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.PersistentCookieJar
import pl.kinowo.net.RepertoireApi
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.Repertoire
import pl.kinowo.ui.detail.DetailPosterTag
import pl.kinowo.ui.theme.KinowoTheme
import java.time.LocalDate
import java.time.ZoneId
import java.time.format.DateTimeFormatter

/**
 * On-device regression for the deep-link film page — the bug that showed up on
 * Xiaomi/MIUI specifically because MIUI keeps the app WARM with the previous
 * city's repertoire still loaded. We reproduce that exact condition: warm the
 * app on poznań (whose list does NOT contain the target film), then fire a
 * cross-city film deep link for a warszawa-only title and assert the detail
 * screen actually opens.
 *
 * Before the `loadedCity` gate, the film was matched against the still-loaded
 * poznań list → miss → the city switched but the film page never opened. This
 * test fails on that code and passes now. Hermetic (a fake [RepertoireApi], no
 * network) so it's deterministic on a Test Lab Redmi.
 *
 * Run on a Redmi via `android/scripts/firebase-test.sh`, or locally with
 * `./gradlew app:connectedDebugAndroidTest --tests pl.kinowo.DeepLinkInstrumentedTest`.
 */
@RunWith(AndroidJUnit4::class)
class DeepLinkInstrumentedTest {

    @get:Rule
    val compose = createAndroidComposeRule<ComponentActivity>()

    private val target = "2046" // warszawa-only film, absent from the poznań list

    private fun today() = LocalDate.now(ZoneId.of("Europe/Warsaw")).format(DateTimeFormatter.ISO_DATE)
    private fun showings() = listOf(DayShowings(today(), "label", listOf(CinemaShowings("Multikino", showtimes = listOf(Showtime("23:59"))))))
    private fun film(t: String) = Film(title = t, posterURL = "https://example.invalid/${t}.jpg", showings = showings())

    private fun seededViewModel(context: Context): KinowoViewModel {
        // poznań WITHOUT the target; warszawa WITH it — so an exact match against
        // the warm poznań list (the old behaviour) misses, and only matching the
        // freshly-loaded warszawa list opens the film.
        val byCity = mapOf(
            "poznan" to listOf(film("Inny film"), film("Coś jeszcze")),
            "warszawa" to listOf(film("Władcy Wszechświata"), film(target)),
        )
        val fakeApi = object : RepertoireApi {
            override suspend fun fetchRepertoire(citySlug: String, ifModifiedSince: String?) =
                KinowoApi.Fetched(byCity[citySlug] ?: emptyList(), null, false)
        }
        val http = OkHttpClient()
        val repository = RepertoireRepository(fakeApi, JsonListCache(context.cacheDir, "rep_dl_probe", Film.serializer()))
        val detailsRepository = DetailsRepository(KinowoApi(client = http), JsonListCache(context.cacheDir, "det_dl_probe", FilmDetails.serializer()))
        val authRepository = AuthRepository(http, PersistentCookieJar(context))
        val noop = object : UserStateClient {
            override suspend fun fetchState() = UserSyncState(emptySet(), emptySet())
            override suspend fun putState(state: UserSyncState) {}
        }
        return KinowoViewModel(repository, detailsRepository, UserPreferences(context), authRepository, noop)
    }

    @Test
    fun warmCrossCityFilmLinkOpensTheDetailScreen() {
        val context = ApplicationProvider.getApplicationContext<Context>()
        // Warm on poznań: persist it so the ViewModel's start() collector fetches
        // poznań before the link arrives (the "app already running" condition).
        runBlocking { UserPreferences(context).setCity("poznan") }

        val vm = seededViewModel(context)
        vm.start()
        compose.setContent { KinowoTheme { Repertoire(vm) } }

        // Wait until poznań's repertoire is the loaded one (warm state established).
        compose.waitUntil(timeoutMillis = 10_000) { vm.films.value.any { it.title == "Inny film" } }

        // Fire the cross-city film deep link (warszawa film absent from poznań).
        compose.runOnUiThread { vm.handleDeepLink("kinowo://warszawa/film?title=$target") }

        // The detail screen must open — its poster carries DetailPosterTag, which
        // only the detail route renders. Before the fix this never appeared.
        compose.waitUntil(timeoutMillis = 15_000) {
            compose.onAllNodes(hasTestTag(DetailPosterTag)).fetchSemanticsNodes().isNotEmpty()
        }
        compose.onNodeWithTag(DetailPosterTag).assertIsDisplayed()
    }
}
