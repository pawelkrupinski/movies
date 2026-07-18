package pl.kinowo.ui.list

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.hasScrollAction
import androidx.compose.ui.test.hasText
import androidx.compose.ui.test.isOff
import androidx.compose.ui.test.isToggleable
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performScrollToNode
import androidx.test.core.app.ApplicationProvider

import okhttp3.OkHttpClient
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.TestData
import pl.kinowo.filter.CinemaSection
import pl.kinowo.auth.AuthRepository
import pl.kinowo.auth.UserStateClient
import pl.kinowo.auth.UserSyncState
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.PersistentCookieJar
import pl.kinowo.ui.KinowoViewModel

/**
 * The cinema filter lives in the Filtry sheet — NOT in a pulldown bar under the
 * date bar. This is the Android half of retiring CinemaPillBar / CinemaAreaBar:
 * "Kina" is a collapsible section listing the city's cinemas as checkboxes over
 * the one `disabledCinemas` axis.
 *
 * Fails before the revert: the sheet had no Kina section at all (cinema choice
 * was the top-bar pill row), so "Kina" never appears here.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "pl")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class FiltersSheetCinemaSectionTest {

    @get:Rule
    val compose = createComposeRule()

    // Rows are labelled with the short pill name, not the full cinema name.
    private val muza = CinemaSection.pillName("Kino Muza")
    private val multikino = CinemaSection.pillName("Multikino Poznań")

    private val films = listOf(
        TestData.film(
            "Diuna",
            listOf(
                TestData.day(
                    "2026-07-18",
                    listOf(
                        TestData.cinema("Kino Muza", listOf(TestData.slot("18:00"))),
                        TestData.cinema("Multikino Poznań", listOf(TestData.slot("20:00"))),
                    ),
                ),
            ),
        ),
    )

    private fun viewModel(): KinowoViewModel {
        val context = ApplicationProvider.getApplicationContext<android.content.Context>()
        val http = OkHttpClient()
        val api = KinowoApi(client = http)
        val repository = RepertoireRepository(api, JsonListCache(context.cacheDir, "repertoire", Film.serializer()))
        val detailsRepository = DetailsRepository(api, JsonListCache(context.cacheDir, "details", FilmDetails.serializer()))
        val authRepository = AuthRepository(http, PersistentCookieJar(context))
        val noopStateClient = object : UserStateClient {
            override suspend fun fetchState() = UserSyncState(emptySet(), emptySet())
            override suspend fun putState(state: UserSyncState) {}
        }
        return KinowoViewModel(
            repository,
            detailsRepository,
            UserPreferences(context),
            authRepository,
            noopStateClient,
        )
    }

    /** The section exists and is collapsed until tapped, like the name filters. */
    @Test
    fun kinaIsACollapsibleSectionInTheSheet() {
        compose.setContent { FiltersSheetContent(viewModel(), films = films) }

        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Kina"))
        compose.onNodeWithText("Kina").assertIsDisplayed()

        // Collapsed: the individual cinemas aren't composed yet.
        compose.onNodeWithText(muza).assertDoesNotExist()

        compose.onNodeWithText("Kina").performClick()

        // Expanded: the master toggle plus one row per cinema in the city.
        compose.onNodeWithText("Wszystkie kina").assertExists()
        compose.onNodeWithText(muza).assertExists()
        compose.onNodeWithText(multikino).assertExists()
    }

    /** Unticking a cinema writes it into the one `disabledCinemas` exclusion set. */
    @Test
    fun untickingACinemaExcludesIt() {
        val viewModel = viewModel()
        compose.setContent { FiltersSheetContent(viewModel, films = films) }

        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Kina"))
        compose.onNodeWithText("Kina").performClick()
        compose.onNodeWithText(muza).performClick()

        // The write goes through DataStore on a background dispatcher and comes
        // back via the ViewModel's flow, so wait for the round-trip to show up in
        // the UI (the row's checkbox clearing) before reading the set. Blocking
        // the test thread instead would starve the main looper the write needs.
        compose.waitUntil(5_000) {
            compose.onAllNodes(isToggleable() and isOff()).fetchSemanticsNodes().isNotEmpty()
        }
        assertEquals(setOf("Kino Muza"), viewModel.disabledCinemas.value)
    }

    /**
     * Kina sits among the content filters — below Sortuj, above Wymiar.
     * (Only neighbours that are on screen together are compared:
     * off-screen LazyColumn items aren't composed, so their bounds are stale.)
     */
    @Test
    fun kinaSitsBetweenSortujAndTheFormatFilters() {
        compose.setContent { FiltersSheetContent(viewModel(), films = films) }

        fun top(text: String) =
            compose.onNodeWithText(text).fetchSemanticsNode().boundsInRoot.top

        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Kina"))
        assertTrue("Kina must sit below Sortuj", top("Kina") > top("Sortuj"))
        assertTrue("Kina must sit above the format filters",
            top("Kina") < top("Wymiar"))
    }

}
