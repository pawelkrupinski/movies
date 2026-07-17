package pl.kinowo.ui.list

import androidx.compose.ui.test.assertIsDisplayed
import androidx.compose.ui.test.hasScrollAction
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.hasText
import androidx.compose.ui.test.isDialog
import androidx.compose.ui.test.junit4.createComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performScrollToNode
import androidx.compose.ui.test.performTextInput
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
import pl.kinowo.model.Cities
import pl.kinowo.model.Country
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

    private fun viewModel(vararg hidden: String): KinowoViewModel {
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
        val prefs = UserPreferences(context)
        if (hidden.isNotEmpty()) runBlocking { hidden.forEach { prefs.hide(it) } }
        return KinowoViewModel(repository, detailsRepository, prefs, authRepository, noopStateClient)
    }

    /**
     * Sortuj stays above the "Ukryte filmy" row — unlike iOS, where the hidden
     * row leads. Guards requirement #1 against the row drifting to the top when
     * it became a nav row.
     */
    @Test
    fun sortujSitsAboveTheHiddenFilmsRow() {
        compose.setContent {
            FiltersSheetContent(viewModel("Diuna", "Barbie"), films = emptyList())
        }

        fun top(text: String) =
            compose.onNodeWithText(text).fetchSemanticsNode().boundsInRoot.top

        assertTrue("Sortuj must sit above the Ukryte filmy row",
            top("Sortuj") < top("Ukryte filmy"))
    }

    /**
     * "Ukryte filmy" is a nav row that opens its own card (mirroring iOS), not
     * an inline collapsible: tapping it reveals a search box that narrows the
     * hidden list as you type. Covers requirements #2 and #3.
     */
    @Test
    fun hiddenFilmsRowOpensASearchableCard() {
        compose.setContent {
            FiltersSheetContent(viewModel("Diuna", "Barbie"), films = emptyList())
        }

        // The filter list shows the row but no search box yet.
        compose.onNodeWithText("Szukaj filmu").assertDoesNotExist()
        compose.onNodeWithText("Ukryte filmy").performClick()

        // The card: search box plus every hidden title.
        compose.onNodeWithText("Szukaj filmu").assertIsDisplayed()
        compose.onNodeWithText("Diuna").assertExists()
        compose.onNodeWithText("Barbie").assertExists()

        // Typing narrows the list to matching titles only.
        compose.onNode(hasSetTextAction()).performTextInput("Diu")
        compose.onNodeWithText("Diuna").assertExists()
        compose.onNodeWithText("Barbie").assertDoesNotExist()
    }

    /**
     * The card takes the whole screen: it's presented as a full-screen Dialog
     * over the Filtry sheet (not an in-place swap), so the filter list stays
     * mounted underneath. Fails the old inline/swap rendering, which had no
     * dialog and replaced the filter list.
     */
    @Test
    fun hiddenFilmsCardOpensAsAFullScreenDialogOverTheFilters() {
        compose.setContent {
            FiltersSheetContent(viewModel("Diuna"), films = emptyList())
        }

        // No dialog until the row is tapped.
        compose.onNode(isDialog()).assertDoesNotExist()

        compose.onNodeWithText("Ukryte filmy").performClick()

        // The card is a Dialog (full-screen overlay), and the filter list (Sortuj)
        // stays mounted underneath rather than being replaced.
        compose.onNode(isDialog()).assertExists()
        compose.onNodeWithText("Sortuj").assertExists()
    }

    /** Tapping Wyczyść closes the Filtry sheet (invokes the onClose callback). */
    @Test
    fun wyczyscClosesTheSheet() {
        var closed = false
        compose.setContent {
            FiltersSheetContent(viewModel(), films = emptyList(), onClose = { closed = true })
        }

        compose.onNodeWithText("Wyczyść").performClick()

        assertTrue("Clicking Wyczyść should close the sheet", closed)
    }

    // (Cinema selection moved out of the Filtry sheet into the top-bar pill row —
    // see CinemaPillBarTest. The old Kina-collapsible tests were removed with it.)

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
        val other = Cities.sortedIn("PL").first { it.slug != Cities.DEFAULT.slug }.name
        compose.onNodeWithText(Cities.DEFAULT.name).assertIsDisplayed()
        compose.onNodeWithText(other).assertDoesNotExist()

        // Tapping the field opens the menu, revealing the other cities.
        compose.onNodeWithText(Cities.DEFAULT.name).performClick()
        compose.onNodeWithText(other).assertExists()
    }

    /**
     * The country switch ("Kraj") sits directly above the city switch ("Miasto")
     * — Country over City — mirroring iOS FiltersBar. Its field shows the current
     * country's native name (Poland by default). Fails if the switcher drifts, or
     * regresses to the old top-bar placement that replaced the 🎬 mark.
     */
    @Test
    fun countryPickerSitsDirectlyAboveTheCityPicker() {
        compose.setContent {
            FiltersSheetContent(viewModel(), films = emptyList())
        }

        fun top(text: String) =
            compose.onNodeWithText(text).fetchSemanticsNode().boundsInRoot.top

        // The country field shows Poland's native name; scroll it + Miasto on screen.
        val poland = Country.default.displayName // "Polska"
        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Miasto"))
        assertTrue("Kraj must sit directly above Miasto",
            top(poland) < top("Miasto"))
    }

    /**
     * The country picker is a Material3 ExposedDropdownMenu like the city one:
     * the field shows the active country, the others stay hidden until tapped,
     * and picking is wired to [pl.kinowo.ui.KinowoViewModel.setCountry]. Fails if
     * it eagerly composes every country or regresses to the top-bar pill.
     */
    @Test
    fun countryPickerIsAnExposedDropdown() {
        compose.setContent {
            FiltersSheetContent(viewModel(), films = emptyList())
        }

        val poland = Country.default.displayName // "Polska"
        val uk = Country.all.first { it != Country.default }.displayName // "United Kingdom"

        compose.onNode(hasScrollAction()).performScrollToNode(hasText(poland))

        // The field shows the active country; another stays hidden until opened.
        compose.onNodeWithText(poland).assertIsDisplayed()
        compose.onNodeWithText(uk).assertDoesNotExist()

        // Tapping the field opens the menu, revealing the other countries.
        compose.onNodeWithText(poland).performClick()
        compose.onNodeWithText(uk).assertExists()
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
