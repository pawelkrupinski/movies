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
     * The city picker is a collapsed dropdown (matching iOS), not a row that
     * lays every city out at once: only the active city shows until tapped,
     * tapping reveals the rest, and picking one switches the city. Fails on the
     * old SegmentedChoice, where every city button is composed up front.
     */
    @Test
    fun cityPickerIsACollapsedDropdown() {
        compose.setContent {
            FiltersSheetContent(viewModel(), films = emptyList())
        }

        val list = compose.onNode(hasScrollAction())
        list.performScrollToNode(hasText("Miasto"))

        // Default city (Poznań) shows on the collapsed button; other cities are
        // hidden until the dropdown opens. Białystok sorts first in the
        // alphabetical picker, so it's the one reliably composed at the top of
        // the opened menu (Wrocław would now be near the bottom, off-screen).
        compose.onNodeWithText("Poznań").assertIsDisplayed()
        compose.onNodeWithText("Białystok").assertDoesNotExist()

        // Tapping the button opens the menu, revealing the other cities.
        compose.onNodeWithText("Poznań").performClick()
        compose.onNodeWithText("Białystok").assertIsDisplayed()

        // Picking a city fires onPick (setCity + close), so the menu collapses
        // again — the revealed items disappear.
        compose.onNodeWithText("Białystok").performClick()
        compose.waitForIdle()
        compose.onNodeWithText("Białystok").assertDoesNotExist()
    }
}
