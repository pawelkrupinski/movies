package pl.kinowo.ui.list

import android.content.Context
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onAllNodesWithText
import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
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
import pl.kinowo.ui.theme.KinowoTheme
import java.time.LocalDate
import java.time.ZoneId
import java.time.format.DateTimeFormatter

/**
 * The per-card cinema label is shown on the main listing even when only one
 * cinema is in view. Seeds a SINGLE-cinema repertoire ("Kino Muza") into the
 * real [ListScreen] and asserts the label renders — exactly the case the old
 * `distinctCinemaCount(visible) > 1` gate suppressed, so it fails before the
 * change (the label `Text` was never composed) and passes after.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "pl")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class CinemaLabelAlwaysShownTest {

    @get:Rule
    val compose = createComposeRule()

    // A name no other test disables, so a DataStore `disabledCinemas` left over
    // from another spec in the shared test JVM can't filter this film out.
    private val cinema = "Kino Testowe Solo"

    private fun seedViewModel(): KinowoViewModel {
        val context = ApplicationProvider.getApplicationContext<Context>()
        // Hermetic: the DataStore is process-wide in the Robolectric JVM, so
        // clear any cinema exclusions a prior test persisted before mounting.
        val prefs = UserPreferences(context)
        runBlocking { prefs.setDisabledCinemas(emptySet()); prefs.setHiddenFilms(emptySet()) }
        val zone = ZoneId.of("Europe/Warsaw")
        val today = LocalDate.now(zone).format(DateTimeFormatter.ISO_DATE)
        // A single film, at a SINGLE cinema, late enough to survive past-pruning.
        val all = listOf(
            Film(
                title = "Solo Film",
                posterURL = "https://x/0.jpg",
                showings = listOf(
                    DayShowings(today, "label", listOf(
                        CinemaShowings(cinema, showtimes = listOf(Showtime("23:59"))),
                    )),
                ),
            ),
        )
        val fakeApi = object : RepertoireApi {
            override suspend fun fetchRepertoire(citySlug: String, ifModifiedSince: String?) =
                KinowoApi.Fetched(all, null, false)
        }
        val repository = RepertoireRepository(fakeApi, JsonListCache(context.cacheDir, "rep_label", Film.serializer()))
        runBlocking { repository.reload("warszawa") }
        val http = OkHttpClient()
        val detailsRepository = DetailsRepository(KinowoApi(client = http), JsonListCache(context.cacheDir, "det_label", FilmDetails.serializer()))
        val authRepository = AuthRepository(http, PersistentCookieJar(context))
        val noop = object : UserStateClient {
            override suspend fun fetchState() = UserSyncState(emptySet(), emptySet())
            override suspend fun putState(state: UserSyncState) {}
        }
        return KinowoViewModel(repository, detailsRepository, prefs, authRepository, noop)
    }

    @Test
    fun cinemaLabelIsShownForASingleCinemaListing() {
        compose.setContent { KinowoTheme { ListScreen(seedViewModel(), onOpenFilm = {}) } }
        compose.waitForIdle()

        // The label `Text(cg.cinema)` is only composed when `showCinemaHeaders`
        // is true; a single-cinema listing used to suppress it (zero nodes).
        assertTrue(
            "The cinema label was not rendered for a single-cinema listing",
            compose.onAllNodesWithText(cinema).fetchSemanticsNodes().isNotEmpty(),
        )
    }
}
