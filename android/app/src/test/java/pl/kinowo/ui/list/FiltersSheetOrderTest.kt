package pl.kinowo.ui.list

import pl.kinowo.KinowoViewModelHarness
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
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.data.UserPreferences
import pl.kinowo.model.Cities
import pl.kinowo.R
import pl.kinowo.ui.KinowoViewModel

/**
 * Off-device (Robolectric) Compose test pinning the Filtry section order:
 * "Miasto" (the city switch) is the last filter, sitting below every content
 * filter and right above the sign-in / sign-out account section — matching
 * iOS FiltersBar. Fails if the city picker drifts back to the top of the
 * sheet.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], qualifiers = "pl")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class FiltersSheetOrderTest {

    @get:Rule
    val compose = createComposeRule()

    @get:Rule
    val harness = KinowoViewModelHarness()

    private fun viewModel(vararg hidden: String): KinowoViewModel {
        val prefs = UserPreferences(harness.context)
        if (hidden.isNotEmpty()) runBlocking { hidden.forEach { prefs.hide(it) } }
        // `CitySection` freezes `selectedCity`/`selectedCountryCode` into a
        // `remember {}` snapshot at FIRST composition (deliberately — see its own
        // doc comment) rather than `collectAsState()`, so there is no later
        // recomposition to correct a snapshot taken before these DataStore-backed
        // StateFlows (`KinowoViewModel`'s `.stateIn(Eagerly, null)`) have loaded
        // their real value. DataStore's own `.data` Flow reads the file on
        // `Dispatchers.IO` — off Robolectric's main Looper, so `ComposeTestRule`'s
        // `waitForIdle()` (which only drains the main Looper) can't be relied on
        // to wait for it under a loaded CI runner. Reading both flows here first
        // forces that disk read to finish on THIS (blocking) call, so DataStore's
        // in-memory cache is already warm by the time `KinowoViewModel` starts
        // collecting them — closing the race rather than papering over it with a
        // longer timeout. Regression: `FiltersSheetOrderTest.
        // cityPickerIsAPickAnotherCityButtonNamingTheCityAndCountry` flaked twice
        // in CI (2026-09-14, 2026-09-19) on exactly this snapshot.
        runBlocking {
            prefs.selectedCity.first()
            prefs.selectedCountryCode.first()
        }
        return harness.viewModel(prefs = prefs)
    }

    private fun filmScreening(format: String) = pl.kinowo.TestData.film("Diuna", listOf(
        pl.kinowo.TestData.day("2026-05-22", listOf(
            pl.kinowo.TestData.cinema("Kino", listOf(pl.kinowo.TestData.slot("18:00", format)))))))

    /** "Tylko IMAX" renders only where some loaded showtime is IMAX — in a city
     *  without one it could only blank the list. Same rule as the web and iOS. */
    @Test
    fun imaxToggleHiddenWhenNoLoadedShowtimeIsImax() {
        compose.setContent {
            FiltersSheetContent(viewModel(), films = listOf(filmScreening("2D NAP")))
        }
        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Od godziny"))
        compose.onNodeWithText("Tylko IMAX").assertDoesNotExist()
    }

    @Test
    fun imaxToggleShownWhenALoadedShowtimeIsImax() {
        compose.setContent {
            FiltersSheetContent(viewModel(), films = listOf(filmScreening("IMAX 3D")))
        }
        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Tylko IMAX"))
        compose.onNodeWithText("Tylko IMAX").assertIsDisplayed()
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
     * Under the UK "en" resources the sheet must render fully in English — the
     * chrome that used to be hardcoded Polish (header, Sortuj, the account
     * section) now reads from string resources. Fails before that i18n pass:
     * the literals rendered "Filtry" / "Sortuj" / "Zaloguj się" regardless of
     * locale. Guards against a hardcoded Polish literal creeping back in.
     */
    @Test
    @Config(qualifiers = "en")
    fun rendersEnglishUnderUkLocale() {
        compose.setContent {
            FiltersSheetContent(viewModel(), films = emptyList())
        }

        // Top-of-sheet chrome is in English…
        compose.onNodeWithText("Filters").assertIsDisplayed()
        compose.onNodeWithText("Sort").assertIsDisplayed()
        // …the account section (further down the list) too.
        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Sign in"))
        compose.onNodeWithText("Sign in").assertIsDisplayed()

        // …and the Polish originals are gone.
        compose.onNodeWithText("Filtry").assertDoesNotExist()
        compose.onNodeWithText("Sortuj").assertDoesNotExist()
        compose.onNodeWithText("Zaloguj się").assertDoesNotExist()
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
     * Miasto is now a single "Wybierz inne miasto" entry point back to the
     * first-launch picker — not an inline Kraj/Miasto pair of dropdowns. Its
     * status line names the current city AND country (so dropping the country
     * picker doesn't drop that information), and tapping the button clears the
     * city and closes the sheet, which is what re-gates the app to the chooser.
     */
    @Test
    fun cityPickerIsAPickAnotherCityButtonNamingTheCityAndCountry() {
        var closed = false
        compose.setContent {
            FiltersSheetContent(viewModel(), films = emptyList(), onClose = { closed = true })
        }

        // Status line: current city, current country — the country name that
        // used to live in its own "Kraj" dropdown is folded in here instead.
        // The country half reads from resources (this class's "pl" qualifiers
        // resolve it to "Polska"), not the fixed Country.displayName field.
        val poland = ApplicationProvider.getApplicationContext<android.content.Context>().getString(R.string.country_pl)
        val statusLine = "${Cities.DEFAULT.name}, $poland"
        compose.onNode(hasScrollAction()).performScrollToNode(hasText(statusLine))
        compose.onNodeWithText(statusLine).assertIsDisplayed()

        // No inline city list — the old dropdown's other cities are gone.
        val otherCity = Cities.sortedIn("pl").first { it.slug != Cities.DEFAULT.slug }.name
        compose.onNodeWithText(otherCity).assertDoesNotExist()

        // The button re-arms the gate (KinowoViewModel.pickAnotherCity) and
        // closes the sheet — closing is what's observable synchronously here;
        // clearCity()/awaitExplicitCityPick() themselves are covered at the
        // UserPreferences layer (UserPreferencesCityTest / ExplicitPickTest).
        // Scroll it into view itself — it sits a row below `statusLine`, which
        // the scroll above only guaranteed for that row, not for this one.
        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Wybierz inne miasto"))
        compose.onNodeWithText("Wybierz inne miasto").performClick()
        assertTrue("Choosing another city should close the sheet", closed)
    }

    /**
     * Język — the language picker — offers all four localized languages by
     * native name and starts collapsed on the resolved default (Polish under
     * this class's "pl" qualifiers, since no explicit pick has been made yet).
     * Guards [pl.kinowo.ui.list.LanguageSection] rendering the new, independent
     * language picker rather than reusing/duplicating the old country-coupled
     * one.
     */
    @Test
    fun languagePickerShowsAllFourNativeNames() {
        compose.setContent {
            FiltersSheetContent(viewModel(), films = emptyList())
        }

        // Scroll to the picker's own row, not the "Język" header above it: the
        // header landing on the viewport's last line leaves this one off-screen.
        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Polski"))
        compose.onNodeWithText("Polski").assertIsDisplayed()
        compose.onNodeWithText("Deutsch").assertDoesNotExist()

        compose.onNodeWithText("Polski").performClick()

        // "Polski" now appears twice (the still-collapsed-looking button label
        // AND its own menu entry) — assert the other three instead, which don't
        // duplicate.
        compose.onNodeWithText("English").assertExists()
        compose.onNodeWithText("Deutsch").assertExists()
        compose.onNodeWithText("Español").assertExists()
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
