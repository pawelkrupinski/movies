package pl.kinowo.ui.list

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.hasScrollAction
import androidx.compose.ui.test.hasText
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performScrollToNode
import androidx.test.core.app.ApplicationProvider
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
import pl.kinowo.model.Cities
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.PersistentCookieJar
import pl.kinowo.ui.KinowoViewModel

/**
 * Off-device (Robolectric) Compose test pinning the Filtry section order:
 * "Miasto" (the city switch) is the last filter, sitting below every content
 * filter and right above the sign-in / sign-out account section — matching
 * iOS FiltersBar. Fails if the city picker drifts back to the top of the
 * sheet.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class FiltersSheetOrderTest {

    @get:Rule
    val compose = createComposeRule()

    private fun viewModel(): KinowoViewModel {
        val context = ApplicationProvider.getApplicationContext<android.content.Context>()
        val http = OkHttpClient()
        val api = KinowoApi(client = http)
        val repo = RepertoireRepository(api, JsonListCache(context.cacheDir, "repertoire", Film.serializer()))
        val detailsRepo = DetailsRepository(api, JsonListCache(context.cacheDir, "details", FilmDetails.serializer()))
        val authRepo = AuthRepository(http, PersistentCookieJar(context))
        val noopStateClient = object : UserStateClient {
            override suspend fun fetchState() = UserSyncState(emptySet(), emptySet())
            override suspend fun putState(state: UserSyncState) {}
        }
        return KinowoViewModel(repo, detailsRepo, UserPreferences(context), authRepo, noopStateClient)
    }

    @Test
    fun cityPickerSitsBelowTheFiltersAndAboveTheAccountSection() {
        compose.setContent {
            FiltersSheetContent(viewModel(), films = emptyList())
        }

        fun top(text: String) =
            compose.onNodeWithText(text).fetchSemanticsNode().boundsInRoot.top

        // Off-screen items in a LazyColumn aren't composed, so scroll each
        // anchor into view before measuring; only compare neighbours that are
        // on screen together. "Miasto" sits between the last content filter
        // ("Od godziny") and the account section ("Zaloguj się").
        val list = compose.onNode(hasScrollAction())

        list.performScrollToNode(hasText("Miasto"))
        assertTrue("Miasto must be the last filter — below 'Od godziny'",
            top("Miasto") > top("Od godziny"))

        list.performScrollToNode(hasText("Zaloguj się"))
        assertTrue("Miasto must sit above the account / sign-in section",
            top("Miasto") < top("Zaloguj się"))
    }

    /**
     * The city picker is a Material3 ExposedDropdownMenu: the field shows the
     * active city, the other cities stay hidden until the field is tapped, and
     * picking one switches the city (collapsing the menu). Fails if the field
     * eagerly composes every city, or the picker regresses to the wheel.
     */
    @Test
    fun cityPickerIsAnExposedDropdown() {
        compose.setContent {
            FiltersSheetContent(viewModel(), films = emptyList())
        }

        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Miasto"))

        // The field shows the active city; another city is hidden until opened.
        val other = Cities.allSorted.first { it.slug != Cities.DEFAULT.slug }.name
        compose.onNodeWithText(Cities.DEFAULT.name).assertIsDisplayed()
        compose.onNodeWithText(other).assertDoesNotExist()

        // Tapping the field opens the menu, revealing the other cities.
        compose.onNodeWithText(Cities.DEFAULT.name).performClick()
        compose.onNodeWithText(other).assertExists()
    }

    /**
     * Od godziny stays a pair of (window-centred) dropdowns: the hour shows
     * "Dowolna" collapsed, and tapping it opens the menu with the hour options.
     * Guards the shared [Dropdown] component the city picker no longer uses.
     */
    @Test
    fun hourPickerIsACollapsedDropdown() {
        compose.setContent {
            FiltersSheetContent(viewModel(), films = emptyList())
        }

        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Od godziny"))

        // Collapsed: the hour button reads "Dowolna"; the hour options are
        // hidden until tapped.
        compose.onNodeWithText("Dowolna").assertIsDisplayed()
        compose.onNodeWithText("06").assertDoesNotExist()

        compose.onNodeWithText("Dowolna").performClick()
        compose.onNodeWithText("06").assertExists()
    }
}
