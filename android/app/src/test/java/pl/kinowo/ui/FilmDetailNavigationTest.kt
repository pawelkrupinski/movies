package pl.kinowo.ui

import android.net.Uri
import androidx.compose.ui.test.hasTestTag
import androidx.compose.ui.test.hasText
import androidx.compose.ui.test.junit4.createComposeRule
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import pl.kinowo.KinowoViewModelHarness
import pl.kinowo.StaticRepertoireApi
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Showtime
import pl.kinowo.ui.detail.DetailPosterTag
import pl.kinowo.ui.theme.KinowoTheme
import java.time.LocalDate
import java.time.ZoneId
import java.time.format.DateTimeFormatter

/**
 * The film detail route carries the film's title, and the detail screen finds
 * its film by EXACT title — so a title the route mangles (a `/` read as a path
 * segment, a `?` as a query, a `%` decoded twice, a space or diacritic
 * re-encoded) opens an empty detail screen. Drives the real nav graph
 * ([Repertoire]) through the real deep-link path (`handleDeepLink` →
 * `pendingFilmNav` → navigate), and asserts the detail screen shows the film.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class FilmDetailNavigationTest {

    @get:Rule(order = 0)
    val harness = KinowoViewModelHarness()

    @get:Rule(order = 1)
    val compose = createComposeRule()

    private val awkwardTitle = "AC/DC: Czy to 100% prawda? Zażółć gęślą jaźń #2 & co"

    private fun today() = LocalDate.now(ZoneId.of("Europe/Warsaw")).format(DateTimeFormatter.ISO_DATE)
    private fun film(title: String) = Film(
        title = title,
        posterURL = "https://example.invalid/poster.jpg",
        showings = listOf(DayShowings(today(), "label", listOf(CinemaShowings("Multikino", showtimes = listOf(Showtime("23:59")))))),
    )

    @Test
    fun aTitleWithRouteSyntaxAndDiacriticsArrivesExactlyAtTheDetailScreen() {
        runBlocking { UserPreferences(harness.context).setCityInCountry("warszawa", "pl") }
        val repository = RepertoireRepository(
            StaticRepertoireApi(listOf(film("Inny film"), film(awkwardTitle))),
            JsonListCache(harness.context.cacheDir, "film_detail_navigation", Film.serializer()),
        )
        val viewModel = harness.viewModel(repository = repository)
        viewModel.start()
        compose.setContent { KinowoTheme { Repertoire(viewModel) } }
        compose.waitUntil(timeoutMillis = 10_000) { viewModel.films.value.any { it.title == awkwardTitle } }

        compose.runOnUiThread { viewModel.handleDeepLink("kinowo://warszawa/movie?title=${Uri.encode(awkwardTitle)}") }

        // The poster carries DetailPosterTag only when the route's title found its film.
        compose.waitUntil(timeoutMillis = 10_000) {
            compose.onAllNodes(hasTestTag(DetailPosterTag)).fetchSemanticsNodes().isNotEmpty()
        }
        assertTrue(compose.onAllNodes(hasText(awkwardTitle)).fetchSemanticsNodes().isNotEmpty())
    }
}
